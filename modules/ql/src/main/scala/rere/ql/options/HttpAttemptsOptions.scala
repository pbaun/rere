package rere.ql.options

import rere.ql.types.ReqlInteger

trait HttpAttemptsOptions {

  sealed trait HttpAttemptsOptions extends ComposableOptions

  case object WithDefaultHttpAttempts extends HttpAttemptsOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class WithHttpAttempts(attempts: ReqlInteger) extends HttpAttemptsOptions {
    def isEmpty = false
    def view = "attempts" -> attempts :: Nil
    def expr = exprFromView
  }

}
