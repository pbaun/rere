package rere.driver.pool

trait PoolShutdownResult {
  def queriesStarted: Long
  def connectionsTurnedOff: Long
}
