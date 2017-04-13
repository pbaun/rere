package rere.ql.options

import rere.ql.queries.values

trait DistanceUnitOptions {

  sealed trait DistanceUnitOptions extends ComposableOptions

  case object DefaultDistanceUnit extends DistanceUnitOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object Meter extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("m") :: Nil
    val expr = exprFromView
  }

  case object Kilometer extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("km") :: Nil
    val expr = exprFromView
  }

  case object InternationalMile extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("mi") :: Nil
    val expr = exprFromView
  }

  case object NauticalMile extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("nm") :: Nil
    val expr = exprFromView
  }

  case object InternationalFoot extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("ft") :: Nil
    val expr = exprFromView
  }

}
