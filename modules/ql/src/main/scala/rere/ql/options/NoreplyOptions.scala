package rere.ql.options

import rere.ql.queries.values

trait NoreplyOptions {

  sealed trait NoreplyOptions extends ComposableOptions

  case object DefaultNoreply extends NoreplyOptions with DefaultOption

  case object Noreply extends NoreplyOptions with NonDefaultOption {
    def view = "noreply" -> values.expr(true) :: Nil
  }

  case object Reply extends NoreplyOptions with NonDefaultOption {
    def view = "noreply" -> values.expr(false) :: Nil
  }

}
