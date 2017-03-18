package rere.ql.options

import rere.ql.queries.values

trait NoreplyOptions {

  sealed trait NoreplyOptions extends ComposableOptions

  case object DefaultNoreply extends NoreplyOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object Noreply extends NoreplyOptions {
    def isEmpty = false
    def view = "noreply" -> values.expr(true) :: Nil
    val innerQuery = query
  }

  case object Reply extends NoreplyOptions {
    def isEmpty = false
    def view = "noreply" -> values.expr(false) :: Nil
    val innerQuery = query
  }

}
