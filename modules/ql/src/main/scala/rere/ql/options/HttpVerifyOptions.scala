package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlBoolean

trait HttpVerifyOptions {

  sealed trait HttpVerifyOptions extends ComposableOptions

  case object WithDefaultHttpVerify extends HttpVerifyOptions with DefaultOption

  case object DoVerify extends HttpVerifyOptions with NonDefaultOption {
    def view = "verify" -> values.expr(true) :: Nil
  }

  case object DoNotVerify extends HttpVerifyOptions with NonDefaultOption {
    def view = "verify" -> values.expr(false) :: Nil
  }

  case class WithHttpVerify(verify: ReqlBoolean) extends HttpVerifyOptions with NonDefaultOption {
    def view = "verify" -> verify :: Nil
  }

}
