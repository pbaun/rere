package rere.ql.options

import rere.ql.types.ReqlInteger

trait HttpAttemptsOptions {

  sealed trait HttpAttemptsOptions extends ComposableOptions

  case object WithDefaultHttpAttempts extends HttpAttemptsOptions with DefaultOption

  case class WithHttpAttempts(attempts: ReqlInteger) extends HttpAttemptsOptions with NonDefaultOption {
    def view = "attempts" -> attempts :: Nil
  }

}
