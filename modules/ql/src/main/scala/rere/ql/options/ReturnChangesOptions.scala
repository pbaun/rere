package rere.ql.options

import rere.ql.queries.values

trait ReturnChangesOptions {

  sealed trait ReturnChangesOptions extends ComposableOptions

  case object DoNotReturnChanges extends ReturnChangesOptions with DefaultOption

  case object DoReturnChanges extends ReturnChangesOptions with NonDefaultOption {
    def view = "return_changes" -> values.expr(true) :: Nil
  }

  case object AlwaysReturnChanges extends ReturnChangesOptions with NonDefaultOption {
    def view = "return_changes" -> values.expr("always") :: Nil
  }

}
