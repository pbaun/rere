package rere.ql.options

import rere.ql.queries.values

trait OverwriteOptions {

  sealed trait OverwriteOptions extends ComposableOptions

  case object NotOverwrite extends OverwriteOptions with DefaultOption

  case object Overwrite extends OverwriteOptions with NonDefaultOption {
    def view = "overwrite" -> values.expr(true) :: Nil
  }

}
