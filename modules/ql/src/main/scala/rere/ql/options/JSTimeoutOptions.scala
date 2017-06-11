package rere.ql.options

import rere.ql.types.ReqlNumber

trait JSTimeoutOptions {

  sealed trait JSTimeoutOptions extends ComposableOptions

  case object WithDefaultJSTimeout extends JSTimeoutOptions with DefaultOption

  case class WithJSTimeout(seconds: ReqlNumber) extends JSTimeoutOptions with NonDefaultOption {
    def view = "timeout" -> seconds :: Nil
  }

}
