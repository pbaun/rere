package rere.driver.workers

import akka.event.LoggingAdapter
import akka.stream.stage._
import akka.stream.{Attributes, Inlet, Outlet}
import rere.driver.connection.LogicalConnectionProtocol
import rere.driver.connection.LogicalConnectionProtocol.{RawResponse, RenderedCommand}
import rere.driver.exceptions.{ReqlDecodeError, ReqlDriverError, ReqlQueryFailedException}
import rere.driver.protocol.{ReqlCommand, ReqlResponse, ReqlStartCommand}
import rere.ql.options.Options
import rere.ql.ql2.Response.ResponseType
import rere.ql.types.ReqlExpr
import rere.ql.wire.ReqlDecoder

import scala.concurrent.{Future, Promise}

class AtomQueryWorker[Expr <: ReqlExpr, Out](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out],
    logger: LoggingAdapter
  ) extends GraphStageWithMaterializedValue[AtomWorkerShape[
    RawResponse,
    RenderedCommand
  ], Future[Out]] {

  private val dbResponses = Inlet[RawResponse]("AtomQueryWorker.dbResponses")
  private val commands = Outlet[RenderedCommand]("AtomQueryWorker.commands")

  val shape = new AtomWorkerShape(dbResponses, commands)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Future[Out]) = {
    val resultPromise = Promise[Out]()
    val logic = new GraphStageLogic(shape) {

      var notStarted = true
      var waitingForResponse = false

      override def preStart(): Unit = {
        logger.debug("AtomQueryWorker.preStart")
      }

      override def postStop(): Unit = {
        logger.debug("AtomQueryWorker.postStop")
        resultPromise.tryFailure(new NoSuchElementException)
        ()
      }

      private def tryToCompleteWithResult(result: Out): Unit = {
        resultPromise.trySuccess(result)
        completeStage()
      }

      private def tryToCompleteWithError(cause: Throwable): Unit = {
        resultPromise.tryFailure(cause)
        failStage(cause)
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
                  response.data match {
                    case Vector(data) =>
                      reqlDecoder.decode(data) match {
                        case Right(out) =>
                          tryToCompleteWithResult(out)

                        case Left(error) =>
                          tryToCompleteWithError(new ReqlDecodeError(
                            s"Can't decode atom element. Query will be finished with error. $error"
                          ))
                      }

                    case _ =>
                      tryToCompleteWithError(new ReqlDecodeError(
                        "Response contains not exactly 1 element. Query will be finished with error."
                      ))
                  }

                case ResponseType.SUCCESS_SEQUENCE | ResponseType.SUCCESS_PARTIAL =>
                  tryToCompleteWithError(new ReqlDriverError(
                    "Sequence received but single value expected"
                  ))

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
    }

    (logic, resultPromise.future)
  }

}
