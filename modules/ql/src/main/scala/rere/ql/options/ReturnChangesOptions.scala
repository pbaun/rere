package rere.ql.options

import rere.ql.queries.values

trait ReturnChangesOptions {

  sealed trait ReturnChangesOptions extends ComposableOptions

  case object DoNotReturnChanges extends ReturnChangesOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object DoReturnChanges extends ReturnChangesOptions {
    def isEmpty = false
    def view = "return_changes" -> values.expr(true) :: Nil
    val innerQuery = query
  }

  case object AlwaysReturnChanges extends ReturnChangesOptions {
    def isEmpty = false
    def view = "return_changes" -> values.expr("always") :: Nil
    val innerQuery = query
  }

}
