package rere.ql.options

import rere.ql.queries.values

trait IndexTypeOptions {

  sealed trait IndexMultiplicityOptions extends ComposableOptions

  case object SimpleIndex extends IndexMultiplicityOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object MultiIndex extends IndexMultiplicityOptions {
    def isEmpty = false
    def view = "multi" -> values.expr(true) :: Nil
    def innerQuery = query
  }

  sealed trait IndexNatureOptions extends ComposableOptions

  case object RangeIndex extends IndexNatureOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object GeoIndex extends IndexNatureOptions {
    def isEmpty = false
    def view = "geo" -> values.expr(true) :: Nil
    def innerQuery = query
  }

}
