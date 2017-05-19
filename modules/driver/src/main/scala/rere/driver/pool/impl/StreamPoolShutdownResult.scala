package rere.driver.pool.impl

import rere.driver.pool.PoolShutdownResult

case class StreamPoolShutdownResult(
    queriesStarted: Long,
    connectionsTurnedOff: Long
  ) extends PoolShutdownResult
