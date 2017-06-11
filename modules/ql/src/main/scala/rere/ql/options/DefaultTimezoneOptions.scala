package rere.ql.options

import rere.ql.types.ReqlString

trait DefaultTimezoneOptions {

  sealed trait DefaultTimezoneOptions extends ComposableOptions

  case object WithoutTimezone extends DefaultTimezoneOptions with DefaultOption

  case class DefaultTimezone(offset: ReqlString) extends DefaultTimezoneOptions with NonDefaultOption {
    def view = "default_timezone" -> offset :: Nil
  }

}
