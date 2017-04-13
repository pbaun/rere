package rere.ql.options

import rere.ql.types.ReqlNumber

trait HttpTimeoutOptions {

  sealed trait HttpTimeoutOptions extends ComposableOptions

  case object WithDefaultHttpTimeout extends HttpTimeoutOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class WithHttpTimeout(seconds: ReqlNumber) extends HttpTimeoutOptions {
    def isEmpty = false
    def view = "timeout" -> seconds :: Nil
    def expr = exprFromView
  }

}
