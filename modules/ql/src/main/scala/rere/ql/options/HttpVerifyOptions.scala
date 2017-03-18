package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlBoolean

trait HttpVerifyOptions {

  sealed trait HttpVerifyOptions extends ComposableOptions

  case object WithDefaultHttpVerify extends HttpVerifyOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object DoVerify extends HttpVerifyOptions {
    def isEmpty = false
    def view = "verify" -> values.expr(true) :: Nil
    def innerQuery = query
  }

  case object DoNotVerify extends HttpVerifyOptions {
    def isEmpty = false
    def view = "verify" -> values.expr(false) :: Nil
    def innerQuery = query
  }

  case class WithHttpVerify(verify: ReqlBoolean) extends HttpVerifyOptions {
    def isEmpty = false
    def view = "verify" -> verify :: Nil
    def innerQuery = query
  }

}
