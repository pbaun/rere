package rere.driver.workers

import akka.actor.{Actor, ActorRef, Props}
import rere.driver.connection.LogicalConnectionProtocol
import rere.driver.exceptions.{ReqlDecodeError, ReqlDriverError, ReqlQueryFailedException, ReqlQueryShutdownException}
import rere.driver.logger.{ATOM_WORKER, Logger}
import rere.driver.protocol._
import rere.ql.options.Options
import rere.ql.ql2.Response.ResponseType
import rere.ql.types.ReqlExpr
import rere.ql.wire.ReqlDecoder

import scala.concurrent.Promise

class AtomQueryWorker[Expr <: ReqlExpr, Out](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out],
    resultPromise: Promise[Out],
    connectionRef: ActorRef,
    logger: Logger
  ) extends Actor {

  override def preStart(): Unit = {
    logger.log(ATOM_WORKER, self.toString(), "Start done")
    val command = new ReqlStartCommand[Expr](expr, runOptions)
    connectionRef ! LogicalConnectionProtocol.RenderedCommand(ReqlCommand.render(command), generateToken = true)
  }

  override def postStop(): Unit = {
    context.parent ! QueryWorkerProtocol.ShutdownComplete(connectionRef, self)
    logger.log(ATOM_WORKER, self.toString(), "Stop done")
  }

  override def receive: Receive = {
    case rawResponse: LogicalConnectionProtocol.RawResponse =>
      ReqlResponse.decode(rawResponse.body) match {
        case Left(failure) =>
          resultPromise.tryFailure(new ReqlDecodeError(
            s"Can't decode response. Query will be finished with error. $failure"
          ))

        case Right(response) =>
          logger.log(ATOM_WORKER, self.toString(), response.toString)

          response.responseType match {
            case ResponseType.SUCCESS_ATOM =>
              response.data match {
                case data :: _ =>
                  reqlDecoder.decode(data) match {
                    case Right(out) =>
                      resultPromise.trySuccess(out)

                    case Left(error) =>
                      resultPromise.tryFailure(new ReqlDecodeError(
                        s"Can't decode atom element. Query will be finished with error. $error"
                      ))
                  }

                case _ =>
                  resultPromise.tryFailure(new ReqlDecodeError(
                    "Response contains not exactly 1 element. Query will be finished with error."
                  ))
              }

            case ResponseType.SUCCESS_SEQUENCE | ResponseType.SUCCESS_PARTIAL =>
              resultPromise.tryFailure(new ReqlDriverError(
                 "Sequence received but single value expected"
              ))

            case ResponseType.WAIT_COMPLETE =>
              resultPromise.tryFailure(new ReqlDriverError(
                "Queries with noReply option are not supported by driver."
              ))

            case _ =>
              val cause = new ReqlQueryFailedException(ReqlResponse.determineError(response))
              resultPromise.tryFailure(cause)
          }
      }
      context.stop(self)

    case QueryWorkerProtocol.ShutdownNow =>
      logger.log(ATOM_WORKER, self.toString(), "Will shutdown now")
      val cause = new ReqlQueryShutdownException()
      resultPromise.tryFailure(cause)
      context.stop(self)

    case QueryWorkerProtocol.ConnectionLost(cause) =>
      logger.log(ATOM_WORKER, self.toString(), s"Connection lost: $cause")
      resultPromise.tryFailure(cause)
      context.stop(self)
  }

}

object AtomQueryWorker {
  def props[Expr <: ReqlExpr, Out](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out],
    resultPromise: Promise[Out],
    connectionRef: ActorRef,
    logger: Logger
  ): Props = {
    Props(new AtomQueryWorker(expr, runOptions, reqlDecoder, resultPromise, connectionRef, logger))
  }
}
