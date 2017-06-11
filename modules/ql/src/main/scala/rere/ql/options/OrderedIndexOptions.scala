package rere.ql.options

import rere.ql.types.ReqlNameOrdering

trait OrderedIndexOptions {

  sealed trait OrderedIndexOptions extends ComposableOptions

  case class OrderedIndex(ordering: ReqlNameOrdering) extends OrderedIndexOptions with NonDefaultOption {
    def view = "index" -> ordering :: Nil
  }

}
