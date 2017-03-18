package rere.ql.options

import rere.ql.queries.values

trait IndexOptions {
  sealed trait IndexOptions extends ComposableOptions

  // index is inherited from previous operators: specific from .orderBy; primary from table???
  case object DefaultIndex extends IndexOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class Index(index: String) extends IndexOptions {
    def isEmpty = false
    def view = "index" -> values.expr(index) :: Nil
    def innerQuery = query
  }
}
