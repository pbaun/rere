package rere.driver.logger

trait Logger {
  def log(group: LoggingGroup, id: String, message: String): Unit
}

sealed trait LoggingGroup
final case object POOL extends LoggingGroup
final case object LOGICAL_CONNECTION extends LoggingGroup
final case object ATOM_WORKER extends LoggingGroup
final case object STREAM_WORKER extends LoggingGroup
final case object HEARTBEAT_WORKER extends LoggingGroup
