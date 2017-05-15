package rere.driver.workers

import akka.Done
import akka.event.LoggingAdapter
import akka.stream.stage._
import akka.stream.{Attributes, Inlet, Outlet, Shape}
import rere.driver.connection.LogicalConnectionProtocol
import rere.driver.connection.LogicalConnectionProtocol.{RawResponse, RenderedCommand}
import rere.driver.exceptions.ReqlDriverError
import rere.driver.protocol.{ReqlCommand, ReqlResponse, ReqlStartCommand}
import rere.driver.workers.QueryWorkerProtocol.ConnectionLost
import rere.ql.options.Options
import rere.ql.ql2.Response.ResponseType
import rere.ql.queries.values
import rere.ql.wire.ReqlDecoder

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}

class HeartbeatWorker(
    firstCheckDelay: FiniteDuration,
    checkInterval: FiniteDuration,
    connectionLostPromise: Promise[ConnectionLost],
    logger: LoggingAdapter
  ) extends GraphStageWithMaterializedValue[AtomWorkerShape[
    RawResponse,
    RenderedCommand
  ], Future[Done]] {

  private type Out = Boolean
  private val reqlDecoder = ReqlDecoder.booleanReqlDecoder

  private case object SendAgain

  private val dbResponses = Inlet[RawResponse]("HeartbeatWorker.dbResponses")
  private val commands = Outlet[RenderedCommand]("HeartbeatWorker.commands")

  val shape = new AtomWorkerShape(dbResponses, commands)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Future[Done]) = {
    val donePromise = Promise[Done]()
    val logic = new TimerGraphStageLogic(shape) {

      var waitingForResponse = false

      override def preStart(): Unit = {
        logger.debug("HeartbeatWorker.preStart")
        schedulePeriodicallyWithInitialDelay(SendAgain, firstCheckDelay, checkInterval)
      }

      override def postStop(): Unit = {
        logger.debug("HeartbeatWorker.postStop")
        donePromise.trySuccess(Done)
        ()
      }

      override protected def onTimer(timerKey: Any): Unit = {
        timerKey match {
          case SendAgain => trySendCheck()

          case _ =>
        }
      }

      private def tryToCompleteWithError(cause: Throwable): Unit = {
        donePromise.tryFailure(cause)
        connectionLostPromise.tryFailure(cause)
        failStage(cause)
      }

      private def reportError(): Unit = {
        logger.warning("Heartbeat check failed")
        val cause = new ReqlDriverError("Heartbeat check failed")
        tryToCompleteWithError(cause)
      }

      private def trySendCheck(): Unit = {
        if (!waitingForResponse) {
          if (isAvailable(commands)) {
            val command = new ReqlStartCommand(values.expr(true), Options.empty)
            val rendered = LogicalConnectionProtocol.RenderedCommand(ReqlCommand.render(command))
            waitingForResponse = true
            push(commands, rendered)
            pull(dbResponses)
          } else {
            logger.warning("Heartbeat check backpressured")
            reportError()
          }
        } else {
          reportError()
        }
      }

      setHandler(dbResponses, new InHandler {
        override def onPush(): Unit = {
          logger.debug("dbResponses.onPush")
          waitingForResponse = false
          val rawResponse = grab(dbResponses)

          ReqlResponse.decode(rawResponse.body) match {
            case Right(response) =>
              response.responseType match {
                case ResponseType.SUCCESS_ATOM =>
                  response.data match {
                    case Vector(data) =>
                      reqlDecoder.decode(data) match {
                        case Right(isAlive) =>
                          if (!isAlive) {
                            reportError()
                          }

                        case Left(_) =>
                          reportError()
                      }

                    case _ =>
                      reportError()
                  }

                case _ =>
                  reportError()
              }

            case Left(_) =>
              reportError()
          }
        }
      })

      setHandler(commands, new OutHandler {
        override def onPull(): Unit = {
          logger.debug("commands.onPull")

          //trySendCheck()
        }

        override def onDownstreamFinish(): Unit = {
          logger.debug("commands.onDownstreamFinish; waitingForResponse = {}", waitingForResponse)
          if (waitingForResponse) {
            failStage(new IllegalStateException("Premature finish"))
          } else {
            completeStage()
          }
        }
      })
    }

    (logic, donePromise.future)
  }

}

class AtomWorkerShape[-In1, +Out1](
    val dbResponses: Inlet[In1 @uncheckedVariance],
    val commands: Outlet[Out1 @uncheckedVariance]
  ) extends Shape {

  override val inlets: immutable.Seq[Inlet[_]] = List(dbResponses)
  override val outlets: immutable.Seq[Outlet[_]] = List(commands)

  override def deepCopy(): AtomWorkerShape[In1, Out1] = {
    new AtomWorkerShape(dbResponses.carbonCopy(), commands.carbonCopy())
  }

  // Kept for compatibility with akka 2.4
  def copyFromPorts(inlets: immutable.Seq[Inlet[_]], outlets: immutable.Seq[Outlet[_]]): Shape = {
    require(inlets.size == 1, s"proposed inlets [${inlets.mkString(", ")}] do not fit AtomWorkerShape")
    require(outlets.size == 1, s"proposed outlets [${outlets.mkString(", ")}] do not fit AtomWorkerShape")
    new AtomWorkerShape(inlets(0), outlets(0))
  }

  def reversed: Shape = copyFromPorts(inlets.reverse, outlets.reverse)
}