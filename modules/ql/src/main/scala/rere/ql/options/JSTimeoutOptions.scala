package rere.ql.options

import rere.ql.types.ReqlNumber

trait JSTimeoutOptions {

  sealed trait JSTimeoutOptions extends ComposableOptions

  case object WithDefaultJSTimeout extends JSTimeoutOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class WithJSTimeout(seconds: ReqlNumber) extends JSTimeoutOptions {
    def isEmpty = false
    def view = "timeout" -> seconds :: Nil
    def expr = exprFromView
  }

}
