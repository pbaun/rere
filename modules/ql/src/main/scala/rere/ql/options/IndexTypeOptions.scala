package rere.ql.options

import rere.ql.queries.values

trait IndexTypeOptions {

  sealed trait IndexMultiplicityOptions extends ComposableOptions

  case object SimpleIndex extends IndexMultiplicityOptions with DefaultOption

  case object MultiIndex extends IndexMultiplicityOptions with NonDefaultOption {
    def view = "multi" -> values.expr(true) :: Nil
  }

  sealed trait IndexNatureOptions extends ComposableOptions

  case object RangeIndex extends IndexNatureOptions with DefaultOption

  case object GeoIndex extends IndexNatureOptions with NonDefaultOption {
    def view = "geo" -> values.expr(true) :: Nil
  }

}
