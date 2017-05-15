package rere.driver.workers

import java.util

import akka.Done
import akka.event.LoggingAdapter
import akka.stream._
import akka.stream.stage._
import io.circe.Json
import rere.driver.connection.LogicalConnectionProtocol
import rere.driver.connection.LogicalConnectionProtocol.{RawResponse, RenderedCommand}
import rere.driver.exceptions.{ReqlDecodeError, ReqlDriverError, ReqlQueryFailedException}
import rere.driver.protocol.{ReqlCommand, ReqlContinueCommand, ReqlResponse, ReqlStartCommand}
import rere.ql.options.Options
import rere.ql.ql2.Response.ResponseType
import rere.ql.types.ReqlExpr
import rere.ql.wire.ReqlDecoder

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable
import scala.concurrent.{Future, Promise}

class StreamQueryWorker[Expr <: ReqlExpr, Out](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out],
    logger: LoggingAdapter
  ) extends GraphStageWithMaterializedValue[StreamWorkerShape[
    RawResponse,
    RenderedCommand, Out
  ], Future[Done]] {

  private val dbResponses = Inlet[RawResponse]("StreamQueryWorker.dbResponses")
  private val commands = Outlet[RenderedCommand]("StreamQueryWorker.commands")
  private val out = Outlet[Out]("StreamQueryWorker.out")

  val shape = new StreamWorkerShape(dbResponses, commands, out)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Future[Done]) = {
    val donePromise = Promise[Done]()
    val logic = new GraphStageLogic(shape) {

      val outBuffer: util.Deque[Out] = new util.ArrayDeque[Out]

      var notStarted = true
      var waitingForResponse = false
      var isTerminating = false

      override def preStart(): Unit = {
        logger.debug("StreamQueryWorker.preStart")
      }

      override def postStop(): Unit = {
        logger.debug("StreamQueryWorker.postStop")
        donePromise.trySuccess(Done)
        ()
      }

      private def tryToCompleteWithError(cause: Throwable): Unit = {
        donePromise.tryFailure(cause)
        failStage(cause)
      }

      private def tryToDecodeAndBuffer(responseData: Vector[Json]): Unit = {
        responseData.foreach { element =>
          reqlDecoder.decode(element) match {
            case Right(outData) =>
              outBuffer.addLast(outData)

            case Left(error) =>
              tryToCompleteWithError(new ReqlDecodeError(
                s"Can't decode stream element. Query will be finished with error. $error"
              ))
              return
          }
        }
      }

      private def tryToPushResponseAndRequestMore(): Unit = {
        val buffered = outBuffer.size()
        if (isAvailable(out) && buffered > 0) {
          push(out, outBuffer.pollFirst())
          val elementsInBuffer = buffered - 1

          logger.debug("pushed = 1; demand = 1; length = {}", elementsInBuffer)

          if (isTerminating && (elementsInBuffer == 0)) {
            completeStage()

            logger.debug("isTerminating && buffer.isEmpty")
          }
        }

        if (!waitingForResponse && !isTerminating && isAvailable(commands) && isAvailable(out)) {
          val command = new ReqlContinueCommand
          push(commands, LogicalConnectionProtocol.RenderedCommand(ReqlCommand.render(command)))
          pull(dbResponses)
          logger.debug("requested more")
          waitingForResponse = true
        }
      }

      setHandler(dbResponses, new InHandler {
        override def onPush(): Unit = {
          logger.debug("dbResponses.onPush")
          waitingForResponse = false
          val rawResponse = grab(dbResponses)

          ReqlResponse.decode(rawResponse.body) match {
            case Left(failure) =>
              tryToCompleteWithError(new ReqlDecodeError(
                s"Can't decode response. Query will be finished with error. $failure"
              ))

            case Right(response) =>
              response.responseType match {
                case ResponseType.SUCCESS_ATOM =>
                  tryToCompleteWithError(new ReqlDriverError(
                    "Single value received but sequence expected"
                  ))

                case ResponseType.SUCCESS_SEQUENCE =>
                  logger.debug("data.size {}", response.data.length)

                  // decode and finish
                  isTerminating = true
                  tryToDecodeAndBuffer(response.data)
                  tryToPushResponseAndRequestMore()

                case ResponseType.SUCCESS_PARTIAL =>
                  logger.debug("data.size {}", response.data.length)

                  // decode and request more
                  tryToDecodeAndBuffer(response.data)
                  tryToPushResponseAndRequestMore()

                case ResponseType.WAIT_COMPLETE =>
                  tryToCompleteWithError(new ReqlDriverError(
                    "Queries with noReply option are not supported by driver."
                  ))

                case _ =>
                  val cause = new ReqlQueryFailedException(ReqlResponse.determineError(response))
                  tryToCompleteWithError(cause)
              }
          }
        }

        override def onUpstreamFinish(): Unit = {
          logger.debug("dbResponses.onUpstreamFinish")
          if (waitingForResponse) {
            tryToCompleteWithError(new IllegalStateException("Premature finish"))
          } else {
            completeStage()
          }
        }

        override def onUpstreamFailure(ex: Throwable): Unit = {
          logger.debug("dbResponses.onUpstreamFailure")
          tryToCompleteWithError(ex)
        }
      })

      setHandler(commands, new OutHandler {
        override def onPull(): Unit = {
          logger.debug("commands.onPull")

          if (notStarted) {
            val command = new ReqlStartCommand[Expr](expr, runOptions)
            val rendered = LogicalConnectionProtocol.RenderedCommand(ReqlCommand.render(command))
            waitingForResponse = true
            push(commands, rendered)
            pull(dbResponses)
            notStarted = false
          } else {
            tryToPushResponseAndRequestMore()
          }
        }

        override def onDownstreamFinish(): Unit = {
          logger.debug("commands.onDownstreamFinish; waitingForResponse = {}", waitingForResponse)
          if (waitingForResponse || notStarted) {
            tryToCompleteWithError(new IllegalStateException("Premature finish"))
          } else {
            completeStage()
          }
        }
      })

      setHandler(out, new OutHandler {
        override def onPull(): Unit = {
          logger.debug("out.onPull")

          tryToPushResponseAndRequestMore()
        }

        override def onDownstreamFinish(): Unit = {
          logger.debug("out.onDownstreamFinish")
          super.onDownstreamFinish()
        }
      })
    }

    (logic, donePromise.future)
  }
}

class StreamWorkerShape[-In1, +Out1, +Out2](
    val dbResponses: Inlet[In1 @uncheckedVariance],
    val commands: Outlet[Out1 @uncheckedVariance],
    val out: Outlet[Out2 @uncheckedVariance]
  ) extends Shape {

  override val inlets: immutable.Seq[Inlet[_]] = List(dbResponses)
  override val outlets: immutable.Seq[Outlet[_]] = List(commands, out)

  override def deepCopy(): StreamWorkerShape[In1, Out1, Out2] = {
    new StreamWorkerShape(dbResponses.carbonCopy(), commands.carbonCopy(), out.carbonCopy())
  }

  // Kept for compatibility with akka 2.4
  def copyFromPorts(inlets: immutable.Seq[Inlet[_]], outlets: immutable.Seq[Outlet[_]]): Shape = {
    require(inlets.size == 1, s"proposed inlets [${inlets.mkString(", ")}] do not fit StreamWorkerShape")
    require(outlets.size == 2, s"proposed outlets [${outlets.mkString(", ")}] do not fit StreamWorkerShape")
    new StreamWorkerShape(inlets(0), outlets(0), outlets(1))
  }

  def reversed: Shape = copyFromPorts(inlets.reverse, outlets.reverse)
}