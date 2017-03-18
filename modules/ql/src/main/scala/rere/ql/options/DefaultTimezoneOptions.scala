package rere.ql.options

import rere.ql.types.ReqlString

trait DefaultTimezoneOptions {

  sealed trait DefaultTimezoneOptions extends ComposableOptions

  case object WithoutTimezone extends DefaultTimezoneOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class DefaultTimezone(offset: ReqlString) extends DefaultTimezoneOptions {
    def isEmpty = false
    def view = "default_timezone" -> offset :: Nil
    val innerQuery = query
  }

}
