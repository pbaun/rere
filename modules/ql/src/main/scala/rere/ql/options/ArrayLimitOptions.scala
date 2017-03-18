package rere.ql.options

import rere.ql.queries.values

trait ArrayLimitOptions {

  sealed trait ArrayLimitOptions extends ComposableOptions

  case object DefaultArrayLimit extends ArrayLimitOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class ArrayLimit(limit: Long) extends ArrayLimitOptions {
    def isEmpty = false
    def view = "array_limit" -> values.expr(limit) :: Nil
    val innerQuery = query
  }

}
