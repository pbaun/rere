package rere.ql.options

import rere.ql.queries.values

trait DryRunOptions {

  sealed trait DryRunOptions extends ComposableOptions

  case object RealRun extends DryRunOptions with DefaultOption

  case object DryRun extends DryRunOptions with NonDefaultOption {
    def view = "dry_run" -> values.expr(true) :: Nil
  }

}
