package rere.driver.logger.impl

import rere.driver.logger.{Logger, LoggingGroup}

class VerboseLogger extends Logger {
  override def log(group: LoggingGroup, id: String, message: String): Unit = {
    println(s"[$group][$id] $message")
  }
}
