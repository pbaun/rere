package rere.ql.options

import rere.ql.queries.values

trait EmergencyRepairOptions {

  sealed trait EmergencyRepairOptions extends ComposableOptions

  case object UnsafeRollback extends EmergencyRepairOptions with NonDefaultOption {
    def view = "emergency_repair" -> values.expr("unsafe_rollback") :: Nil
  }

  case object UnsafeRollbackOrErase extends EmergencyRepairOptions with NonDefaultOption {
    def view = "emergency_repair" -> values.expr("unsafe_rollback_or_erase") :: Nil
  }

}
