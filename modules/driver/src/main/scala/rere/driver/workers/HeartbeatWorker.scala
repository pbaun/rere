package rere.driver.workers

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import rere.driver.connection.LogicalConnectionProtocol
import rere.driver.exceptions.ReqlDriverError
import rere.driver.logger.{HEARTBEAT_WORKER, Logger}
import rere.driver.pool.ConnectionProblem
import rere.driver.protocol.{ReqlCommand, ReqlResponse, ReqlStartCommand}
import rere.ql.options.Options
import rere.ql.ql2.Response.ResponseType
import rere.ql.queries.values
import rere.ql.wire.ReqlDecoder

import scala.concurrent.duration.FiniteDuration

class HeartbeatWorker(
    firstCheckDelay: FiniteDuration,
    checkInterval: FiniteDuration,
    connectionRef: ActorRef,
    logger: Logger
  ) extends Actor {

  private implicit val ec = context.dispatcher

  private case object SendAgain
  private val timer: Cancellable = context.system.scheduler.schedule(firstCheckDelay, checkInterval, self, SendAgain)
  private var waitingForResponse = false

  private val command = new ReqlStartCommand(values.expr(true), Options.empty)
  private val reqlDecoder = ReqlDecoder[Boolean]

  def sendCommand(): Unit = {
    connectionRef ! LogicalConnectionProtocol.RenderedCommand(ReqlCommand.render(command), generateToken = true)
    waitingForResponse = true
  }

  def reportError(): Unit = {
    val cause = new ReqlDriverError("Heartbeat check failed")
    context.parent ! ConnectionProblem(connectionRef, cause)
  }

  override def preStart(): Unit = {
    logger.log(HEARTBEAT_WORKER, self.toString(), "Start done")
  }

  override def postStop(): Unit = {
    context.parent ! QueryWorkerProtocol.ShutdownComplete(connectionRef, self)
    timer.cancel()
    logger.log(HEARTBEAT_WORKER, self.toString(), "Stop done")
  }

  override def receive: Receive = {
    case rawResponse: LogicalConnectionProtocol.RawResponse =>
      waitingForResponse = false
      ReqlResponse.decode(rawResponse.body) match {
        case Right(response) =>
          response.responseType match {
            case ResponseType.SUCCESS_ATOM =>
              response.data match {
                case data :: Nil =>
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

    case SendAgain =>
      if (waitingForResponse) {
        reportError()
      } else {
        sendCommand()
      }

    case QueryWorkerProtocol.ShutdownNow =>
      logger.log(HEARTBEAT_WORKER, self.toString(), "Will shutdown now")
      context.stop(self)

    case QueryWorkerProtocol.ConnectionLost(cause) =>
      logger.log(HEARTBEAT_WORKER, self.toString(), s"Connection lost: $cause")
      context.stop(self)
  }
}

object HeartbeatWorker {
  def props(
    firstCheckDelay: FiniteDuration,
    checkInterval: FiniteDuration,
    connectionRef: ActorRef,
    logger: Logger
  ): Props = {
    Props(new HeartbeatWorker(firstCheckDelay, checkInterval, connectionRef, logger))
  }
}
