package rere.driver.workers

import akka.actor.ActorRef

object QueryWorkerProtocol {
  final case object ShutdownNow
  final case class ShutdownComplete(connectionRef: ActorRef, workerRef: ActorRef)

  final case class ConnectionLost(cause: Throwable)
}
