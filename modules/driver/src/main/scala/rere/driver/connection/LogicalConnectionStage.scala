package rere.driver.connection

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import akka.event.LoggingAdapter
import akka.stream._
import akka.stream.scaladsl.{Flow, GraphDSL, MergeHub, RunnableGraph, Sink, Source, SourceQueueWithComplete}
import akka.stream.stage._
import akka.util.ByteString
import akka.{Done, NotUsed}
import rere.driver.connection.LogicalConnectionProtocol._

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

object LogicalConnection {
  def connect[RealConnectionMat](
    connectionId: Long,
    realConnectionWithFraming: Flow[ReqlFrame, ReqlFrame, RealConnectionMat],
    logger: LoggingAdapter)(
    implicit materializer: Materializer
  ): (RealConnectionMat, LogicalConnection) = {
    val routingTable = new ConcurrentHashMap[Long, SourceQueueWithComplete[RawResponse]]()

    val logicalConnectionStage = new LogicalConnectionStage(routingTable, logger)
    val mergeHub = MergeHub.source[RenderedCommandWithToken]

    val shutdownPromise = Promise[PrepareForShutdown.type]()
    val shutdownSource = Source.fromFuture(shutdownPromise.future)

    val graph = RunnableGraph.fromGraph(GraphDSL.create(
      realConnectionWithFraming, logicalConnectionStage, mergeHub, shutdownSource) {
      (realM, logicalM, mergeHubM, shutdownM) => (realM, logicalM, mergeHubM)
    } { implicit builder =>
      (realConnection, logicalConnection, mergeHub, shutdownSource) =>

        import GraphDSL.Implicits._

        mergeHub ~> logicalConnection.commands
        logicalConnection.framingRequests  ~> realConnection.in
        logicalConnection.framingResponses <~ realConnection.out
        // db response will be delivered to worker using Source.queue that will be created on demand

        shutdownSource ~> logicalConnection.prepareForShutdown

        ClosedShape
    })

    val (realConnectionMat, logicalConnectionDone, mergeSink) = graph.run()

    val logicalConnection =
      new LogicalConnection(connectionId, routingTable, mergeSink, shutdownPromise, logicalConnectionDone, logger)

    (realConnectionMat, logicalConnection)
  }
}

trait Connection {
  def id: Long
  def load: Long
  def prepareForShutdown(): Future[Done]
}

class LogicalConnection private(
    connectionId: Long,
    routingTable: ConcurrentHashMap[Long, SourceQueueWithComplete[RawResponse]],
    mergeSinkRef: Sink[RenderedCommandWithToken, NotUsed],
    shutdownPromise: Promise[PrepareForShutdown.type],
    connectionDone: Future[Done],
    logger: LoggingAdapter
  ) extends Connection {

  private val tokenCounter = new AtomicLong(0L)
  private def acquireToken(): Long = tokenCounter.getAndIncrement()

  private val workersCounter = new AtomicLong(0L)

  def id: Long = connectionId

  def load: Long = workersCounter.get()  // like routingTable.size(), but faster

  def prepareForShutdown(): Future[Done] = {
    logger.debug("Connection shutdown")
    shutdownPromise.trySuccess(PrepareForShutdown)
    connectionDone
  }

  // for debug and tests
  def isBusyToken(token: Long): Boolean = {
    routingTable.containsKey(token)
  }

  // fo debug and tests
  def done: Future[Done] = connectionDone

  def getConnectionFlow(): Flow[RenderedCommand, RawResponse, NotUsed] = {
    val workerToken = acquireToken()
    val responseSource = Source.queue[RawResponse](0, OverflowStrategy.fail)

    Flow.fromGraph(GraphDSL.create(
      responseSource, mergeSinkRef) {
      (responseSourceM, mergeSinkM) => responseSourceM
    } { implicit builder =>
      (responseSource, mergeSink) =>

        import GraphDSL.Implicits._

        // Use the Builder, Luke!
        val blockingRegistrar = builder.add(new WorkerRegistrarMerge(
          routingTable, workersCounter, workerToken, logger
        ))
        val tokenizer = builder.add(Flow[RenderedCommand].map {
          command => RenderedCommandWithToken(workerToken, command.body)
        })

        // registrar will block commands flow until it will get queue reference from materializedValue outlet
        // later queue reference will be used by logical connection stage for deliver response to right worker
        // it's safe to register queue before first command and use later for response routing because ...
        //  - response routing happens before response retrieving by worker
        //  - response retrieving by connection happens before response routing
        //  - command sending happens before response retrieving
        //  - commands flow unlocking happens before command sending
        //  - queue registration happens before commands flow unlocking
        //  - materialized value retrieving happens before queue registration
        // ... so queue registration happens before response routing

        builder.materializedValue ~> blockingRegistrar.matValue

        tokenizer ~> blockingRegistrar.commandsIn
                     blockingRegistrar.commandsOut ~> mergeSink

        FlowShape.of(tokenizer.in, responseSource.out)
    }).mapMaterializedValue(_ => NotUsed)
  }

}

class LogicalConnectionStage(
    routingTable: ConcurrentHashMap[Long, SourceQueueWithComplete[RawResponse]],
    logger: LoggingAdapter
  ) extends GraphStageWithMaterializedValue[LogicalConnectionShape[
    RenderedCommandWithToken, PrepareForShutdown.type, ReqlFrame,
    ReqlFrame
  ], Future[Done]] {

  private val commands = Inlet[RenderedCommandWithToken]("LogicalConnection.commands")
  private val prepareForShutdown = Inlet[PrepareForShutdown.type]("LogicalConnection.prepareForShutdown")
  private val framingResponses = Inlet[ReqlFrame]("LogicalConnection.framingResponses")
  private val framingRequests = Outlet[ReqlFrame]("LogicalConnection.framingRequests")

  val shape = new LogicalConnectionShape(commands, prepareForShutdown, framingResponses, framingRequests)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Future[Done]) = {
    val donePromise = Promise[Done]()
    val logic = new GraphStageLogic(shape) {
      override def preStart(): Unit = {
        logger.debug("LogicalConnection.preStart")

        pull(prepareForShutdown)
      }

      override def postStop(): Unit = {
        logger.debug("LogicalConnection.postStop")
        donePromise.trySuccess(Done)
        ()
      }

      setHandler(commands, new InHandler {
        override def onPush(): Unit = {
          logger.debug("commands.onPush")

          if (isAvailable(framingRequests)) {
            val command = grab(commands)

            if (logger.isDebugEnabled) {
              logger.debug("framing command push: {}; token {}; body {}", command.toString, command.token, command.body.utf8String)
            }

            push(framingRequests, ReqlFrame(command.token, command.body))

            // Cannot pull port (LogicalConnection.framingResponses) twice
            if (!hasBeenPulled(framingResponses)) {
              pull(framingResponses)
            }
          }
        }

        override def onUpstreamFailure(ex: Throwable): Unit = {
          logger.debug("commands.onUpstreamFailure {}", ex)
          super.onUpstreamFailure(ex)
        }

        override def onUpstreamFinish(): Unit = {
          logger.debug("commands.onUpstreamFinish")
          super.onUpstreamFinish()
        }
      })

      setHandler(prepareForShutdown, new InHandler {
        override def onPush(): Unit = {
          logger.debug("prepareForShutdown.onPush")

          completeStage()
        }

        override def onUpstreamFailure(ex: Throwable): Unit = {
          logger.debug("prepareForShutdown.onUpstreamFailure {}", ex)
          super.onUpstreamFailure(ex)
        }

        override def onUpstreamFinish(): Unit = {
          logger.debug("prepareForShutdown.onUpstreamFinish")
          super.onUpstreamFinish()
        }
      })

      setHandler(framingResponses, new InHandler {
        override def onPush(): Unit = {
          logger.debug("framingResponses.onPush")

          val response = grab(framingResponses)
          pull(framingResponses)
          val queue = routingTable.get(response.queryToken)

          if (queue ne null) {
            queue.offer(RawResponse(response.body))
            ()
          }
        }

        override def onUpstreamFailure(ex: Throwable): Unit = {
          logger.debug("framingResponses.onUpstreamFailure {}", ex)
          super.onUpstreamFailure(ex)
        }

        override def onUpstreamFinish(): Unit = {
          logger.debug("framingResponses.onUpstreamFinish")
          super.onUpstreamFinish()
        }
      })

      setHandler(framingRequests, new OutHandler {
        override def onPull(): Unit = {
          logger.debug("framingRequests.onPull")

          if (isAvailable(commands)) {
            val command = grab(commands)

            if (logger.isDebugEnabled) {
              logger.debug("framing command push: {}; token {}; body {}", command.toString, command.token, command.body.utf8String)
            }

            push(framingRequests, ReqlFrame(command.token, command.body))
            pull(framingResponses)
          }

          //Cannot pull port (LogicalConnection.commands) twice
          if (!hasBeenPulled(commands)) {
            pull(commands)
          }
        }

        override def onDownstreamFinish(): Unit = {
          logger.debug("framingRequests.onDownstreamFinish")
          super.onDownstreamFinish()
        }
      })
    }

    (logic, donePromise.future)
  }
}

class LogicalConnectionShape[-In1, -In2, -In3, +Out1](
    val commands: Inlet[In1 @uncheckedVariance],
    val prepareForShutdown: Inlet[In2 @uncheckedVariance],
    val framingResponses: Inlet[In3 @uncheckedVariance],
    val framingRequests: Outlet[Out1 @uncheckedVariance]
  ) extends Shape {

  override val inlets: immutable.Seq[Inlet[_]] = List(commands, prepareForShutdown, framingResponses)
  override val outlets: immutable.Seq[Outlet[_]] = List(framingRequests)

  override def deepCopy(): LogicalConnectionShape[In1, In2, In3, Out1] = {
    new LogicalConnectionShape(
      commands.carbonCopy(), prepareForShutdown.carbonCopy(), framingResponses.carbonCopy(),
      framingRequests.carbonCopy()
    )
  }

  override def copyFromPorts(inlets: immutable.Seq[Inlet[_]], outlets: immutable.Seq[Outlet[_]]): Shape = {
    require(inlets.size == 3, s"proposed inlets [${inlets.mkString(", ")}] do not fit LogicalConnectionShape")
    require(outlets.size == 1, s"proposed outlets [${outlets.mkString(", ")}] do not fit LogicalConnectionShape")
    new LogicalConnectionShape(inlets(0), inlets(1), inlets(2), outlets(0))
  }

  def reversed: Shape = copyFromPorts(inlets.reverse, outlets.reverse)
}


class WorkerRegistrarMerge(
    routingTable: ConcurrentHashMap[Long, SourceQueueWithComplete[RawResponse]],
    workersCounter: AtomicLong,
    workerToken: Long,
    logger: LoggingAdapter
  ) extends GraphStage[WorkerRegistrarMergeShape[
    RenderedCommandWithToken, SourceQueueWithComplete[RawResponse], RenderedCommandWithToken
  ]] {

  val commandsIn = Inlet[RenderedCommandWithToken]("WorkerRegistrarMerge.commandsIn")
  val matValue = Inlet[SourceQueueWithComplete[RawResponse]]("WorkerRegistrarMerge.matValue")
  val commandsOut = Outlet[RenderedCommandWithToken]("WorkerRegistrarMerge.commandsOut")

  val shape = new WorkerRegistrarMergeShape(commandsIn, matValue, commandsOut)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = {
    new GraphStageLogic(shape) {

      private var unlocked = false

      val doneCallback = getAsyncCallback[Try[Done]] {
        case Success(done) =>
          logger.debug("doneCallback.Success({})", done)
          completeStage()
        case Failure(cause) =>
          logger.debug("doneCallback.Failure({})", cause)
          failStage(cause)   //TODO: should we fail connection?
      }

      override def preStart(): Unit = {
        logger.debug("WorkerRegistrarMerge.preStart; token {}", workerToken)
        pull(matValue)
      }

      override def postStop(): Unit = {
        logger.debug("WorkerRegistrarMerge.postStop; token {}", workerToken)
        val queue = routingTable.remove(workerToken)
        if (queue ne null) {
          queue.complete()
          workersCounter.getAndDecrement()
        }
        ()
      }

      setHandler(commandsIn, new InHandler {
        override def onPush(): Unit = {
          logger.debug("commandsIn.onPush")

          if (unlocked) {
            if (isAvailable(commandsOut)) {
              val command = grab(commandsIn)
              if (logger.isDebugEnabled) {
                logger.debug("merge command push: {} token {} body {}", command.toString, command.token, command.body.utf8String)
              }
              push(commandsOut, command)
              pull(commandsIn)
            }
          }
        }
      })

      setHandler(matValue, new InHandler {
        override def onPush(): Unit = {
          logger.debug("matValue.onPush")

          val queue = grab(matValue)
          if (routingTable.putIfAbsent(workerToken, queue) ne null) {
            failStage(new IllegalStateException(s"Token $workerToken already been used"))
          } else {
            workersCounter.getAndIncrement()
            queue.watchCompletion().onComplete(doneCallback.invoke)(materializer.executionContext)
            unlocked = true
            pull(commandsIn)
          }
        }

        override def onUpstreamFinish(): Unit = {
          logger.debug("matValue.onFinish")
          // ignore finish
        }
      })

      setHandler(commandsOut, new OutHandler {
        override def onPull(): Unit = {
          logger.debug("commandsOut.onPull")

          if (unlocked) {
            if (isAvailable(commandsIn)) {
              push(commandsOut, grab(commandsIn))
              pull(commandsIn)
            }
          }
        }
      })
    }
  }
}

class WorkerRegistrarMergeShape[-In1, -In2, +Out1](
    val commandsIn: Inlet[In1 @uncheckedVariance],
    val matValue: Inlet[In2 @uncheckedVariance],
    val commandsOut: Outlet[Out1 @uncheckedVariance]
  ) extends Shape {

  override val inlets: immutable.Seq[Inlet[_]] = List(commandsIn, matValue)
  override val outlets: immutable.Seq[Outlet[_]] = List(commandsOut)

  override def deepCopy(): WorkerRegistrarMergeShape[In1, In2, Out1] = {
    new WorkerRegistrarMergeShape(
      commandsIn.carbonCopy(), matValue.carbonCopy(), commandsOut.carbonCopy()
    )
  }

  override def copyFromPorts(inlets: immutable.Seq[Inlet[_]], outlets: immutable.Seq[Outlet[_]]): Shape = {
    require(inlets.size == 2, s"proposed inlets [${inlets.mkString(", ")}] do not fit WorkerRegistrarMergeShape")
    require(outlets.size == 1, s"proposed outlets [${outlets.mkString(", ")}] do not fit WorkerRegistrarMergeShape")
    new WorkerRegistrarMergeShape(inlets(0), inlets(1), outlets(0))
  }

  def reversed: Shape = copyFromPorts(inlets.reverse, outlets.reverse)
}

object LogicalConnectionProtocol {
  final case class RenderedCommand(body: ByteString)
  final case class RenderedCommandWithToken(token: Long, body: ByteString)

  final case class RawResponse(body: ByteString)

  final case object PrepareForShutdown
}