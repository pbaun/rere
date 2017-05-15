package rere.driver.workers

object QueryWorkerProtocol {
  final case class ConnectionLost(cause: Throwable)
}
