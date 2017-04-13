package rere.ql.options

import rere.ql.queries.values

trait OrderingOptions {

  sealed trait OrderingOptions extends ComposableOptions

  case object NotOrdered extends OrderingOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object Ordered extends OrderingOptions {
    def isEmpty = false
    def view = "ordered" -> values.expr(true) :: Nil
    val expr = exprFromView
  }

}
