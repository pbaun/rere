package rere.ql.options

import rere.ql.queries.values

trait NoreplyOptions {

  sealed trait NoreplyOptions extends ComposableOptions

  case object DefaultNoreply extends NoreplyOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object Noreply extends NoreplyOptions {
    def isEmpty = false
    def view = "noreply" -> values.expr(true) :: Nil
    val expr = exprFromView
  }

  case object Reply extends NoreplyOptions {
    def isEmpty = false
    def view = "noreply" -> values.expr(false) :: Nil
    val expr = exprFromView
  }

}
