package rere.ql.options

import rere.ql.queries.values

trait RandomOptions {

  sealed trait RandomOptions extends ComposableOptions

  case object IntegerValues extends RandomOptions with DefaultOption

  case object FloatValues extends RandomOptions with NonDefaultOption {
    def view = "float" -> values.expr(true) :: Nil
  }

}
