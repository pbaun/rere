package rere.ql.options

import rere.ql.types.ReqlNameOrdering

trait OrderedIndexOptions {

  sealed trait OrderedIndexOptions extends ComposableOptions

  case class OrderedIndex(ordering: ReqlNameOrdering) extends OrderedIndexOptions {
    def isEmpty = false
    def view = "index" -> ordering :: Nil
    def innerQuery = query
  }

}
