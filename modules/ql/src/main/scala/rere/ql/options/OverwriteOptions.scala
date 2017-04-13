package rere.ql.options

import rere.ql.queries.values

trait OverwriteOptions {

  sealed trait OverwriteOptions extends ComposableOptions

  case object NotOverwrite extends OverwriteOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object Overwrite extends OverwriteOptions {
    def isEmpty = false
    def view = "overwrite" -> values.expr(true) :: Nil
    val expr = exprFromView
  }

}
