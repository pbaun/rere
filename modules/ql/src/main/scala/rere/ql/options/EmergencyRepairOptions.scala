package rere.ql.options

import rere.ql.queries.values

trait EmergencyRepairOptions {

  sealed trait EmergencyRepairOptions extends ComposableOptions

  case object UnsafeRollback extends EmergencyRepairOptions {
    def isEmpty = false
    def view = "emergency_repair" -> values.expr("unsafe_rollback") :: Nil
    def innerQuery = query
  }

  case object UnsafeRollbackOrErase extends EmergencyRepairOptions {
    def isEmpty = false
    def view = "emergency_repair" -> values.expr("unsafe_rollback_or_erase") :: Nil
    def innerQuery = query
  }

}
