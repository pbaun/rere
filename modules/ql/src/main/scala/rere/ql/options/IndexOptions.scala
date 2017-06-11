package rere.ql.options

import rere.ql.queries.values

trait IndexOptions {
  sealed trait IndexOptions extends ComposableOptions

  // index is inherited from previous operators: specific from .orderBy; primary from table???
  case object DefaultIndex extends IndexOptions with DefaultOption

  case class Index(index: String) extends IndexOptions with NonDefaultOption {
    def view = "index" -> values.expr(index) :: Nil
  }
}
