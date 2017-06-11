package rere.ql.options

import rere.ql.queries.values

trait ArrayLimitOptions {

  sealed trait ArrayLimitOptions extends ComposableOptions

  case object DefaultArrayLimit extends ArrayLimitOptions with DefaultOption

  case class ArrayLimit(limit: Long) extends ArrayLimitOptions with NonDefaultOption {
    def view = "array_limit" -> values.expr(limit) :: Nil
  }

}
