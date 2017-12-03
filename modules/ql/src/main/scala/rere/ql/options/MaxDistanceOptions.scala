package rere.ql.options

import rere.ql.queries.values

trait MaxDistanceOptions {

  sealed trait MaxDistanceOptions extends ComposableOptions

  case object DefaultMaxDistance extends MaxDistanceOptions with DefaultOption

  case class MaxDistance(n: Int) extends MaxDistanceOptions with NonDefaultOption {
    def view = "max_dist" -> values.expr(n) :: Nil
  }

}
