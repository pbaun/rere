package rere.ql.options

import rere.ql.queries.values

trait DryRunOptions {

  sealed trait DryRunOptions extends ComposableOptions

  case object RealRun extends DryRunOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object DryRun extends DryRunOptions {
    def isEmpty = false
    def view = "dry_run" -> values.expr(true) :: Nil
    val expr = exprFromView
  }

}
