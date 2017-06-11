package rere.ql.options

import rere.ql.queries.values

trait DistanceUnitOptions {

  sealed trait DistanceUnitOptions extends ComposableOptions

  case object DefaultDistanceUnit extends DistanceUnitOptions with DefaultOption

  case object Meter extends DistanceUnitOptions with NonDefaultOption {
    def view = "unit" -> values.expr("m") :: Nil
  }

  case object Kilometer extends DistanceUnitOptions with NonDefaultOption {
    def view = "unit" -> values.expr("km") :: Nil
  }

  case object InternationalMile extends DistanceUnitOptions with NonDefaultOption {
    def view = "unit" -> values.expr("mi") :: Nil
  }

  case object NauticalMile extends DistanceUnitOptions with NonDefaultOption {
    def view = "unit" -> values.expr("nm") :: Nil
  }

  case object InternationalFoot extends DistanceUnitOptions with NonDefaultOption {
    def view = "unit" -> values.expr("ft") :: Nil
  }

}
