package rere.ql.options

import rere.ql.queries.values

trait CircleFillOptions {

  sealed trait CircleFillOptions extends ComposableOptions

  case object DefaultCircleFill extends CircleFillOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object FillCircle extends CircleFillOptions {
    def isEmpty = false
    def view = "fill" -> values.expr(true) :: Nil
    val innerQuery = query
  }

  case object NotFillCircle extends CircleFillOptions {
    def isEmpty = false
    def view = "fill" -> values.expr(false) :: Nil
    val innerQuery = query
  }

}
