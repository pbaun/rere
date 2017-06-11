package rere.ql.options

import rere.ql.queries.values

trait OrderingOptions {

  sealed trait OrderingOptions extends ComposableOptions

  case object NotOrdered extends OrderingOptions with DefaultOption

  case object Ordered extends OrderingOptions with NonDefaultOption {
    def view = "ordered" -> values.expr(true) :: Nil
  }

}
