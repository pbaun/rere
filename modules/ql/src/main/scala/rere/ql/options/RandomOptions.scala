package rere.ql.options

import rere.ql.queries.values

trait RandomOptions {

  sealed trait RandomOptions extends ComposableOptions

  case object IntegerValues extends RandomOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object FloatValues extends RandomOptions {
    def isEmpty = false
    def view = "float" -> values.expr(true) :: Nil
    val expr = exprFromView
  }

}
