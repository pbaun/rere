package rere.ql.options

import rere.ql.queries.values

trait CircleFillOptions {

  sealed trait CircleFillOptions extends ComposableOptions

  case object DefaultCircleFill extends CircleFillOptions with DefaultOption

  case object FillCircle extends CircleFillOptions with NonDefaultOption {
    def view = "fill" -> values.expr(true) :: Nil
  }

  case object NotFillCircle extends CircleFillOptions with NonDefaultOption {
    def view = "fill" -> values.expr(false) :: Nil
  }

}
