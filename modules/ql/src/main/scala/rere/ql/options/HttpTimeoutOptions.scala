package rere.ql.options

import rere.ql.types.ReqlNumber

trait HttpTimeoutOptions {

  sealed trait HttpTimeoutOptions extends ComposableOptions

  case object WithDefaultHttpTimeout extends HttpTimeoutOptions with DefaultOption

  case class WithHttpTimeout(seconds: ReqlNumber) extends HttpTimeoutOptions with NonDefaultOption {
    def view = "timeout" -> seconds :: Nil
  }

}
