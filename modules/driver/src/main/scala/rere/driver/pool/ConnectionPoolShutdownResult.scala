package rere.driver.pool

import java.util.concurrent.TimeoutException

sealed trait ConnectionPoolShutdownResult

final case class ShutdownSuccessfullyDone(
  queriesExecuted: Long,
  connectionsTurnedOff: Int
) extends ConnectionPoolShutdownResult

final case class ShutdownDoneByTimeout() extends ConnectionPoolShutdownResult

class ConnectionPoolShutdownTimeoutException(msg: String) extends TimeoutException(msg)