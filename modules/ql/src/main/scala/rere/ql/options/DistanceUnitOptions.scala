package rere.ql.options

import rere.ql.queries.values

trait DistanceUnitOptions {

  sealed trait DistanceUnitOptions extends ComposableOptions

  case object DefaultDistanceUnit extends DistanceUnitOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object Meter extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("m") :: Nil
    val innerQuery = query
  }

  case object Kilometer extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("km") :: Nil
    val innerQuery = query
  }

  case object InternationalMile extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("mi") :: Nil
    val innerQuery = query
  }

  case object NauticalMile extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("nm") :: Nil
    val innerQuery = query
  }

  case object InternationalFoot extends DistanceUnitOptions {
    def isEmpty = false
    def view = "unit" -> values.expr("ft") :: Nil
    val innerQuery = query
  }

}
