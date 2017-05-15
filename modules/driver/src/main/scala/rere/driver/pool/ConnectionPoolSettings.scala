package rere.driver.pool

import scala.concurrent.duration._

case class ConnectionPoolSettings(
  poolName: String,
  poolSize: Int,
  backoffStep: FiniteDuration = 500.millis,
  backoffMaxSteps: Long = 5L,
  heartbeatInitialDelay: FiniteDuration = 3.seconds,
  heartbeatCheckInterval: FiniteDuration = 1.second
)
