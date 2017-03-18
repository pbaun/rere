package rere.ql.options

import rere.ql.queries.values

trait MaxDistanceOptions {

  sealed trait MaxDistanceOptions extends ComposableOptions

  case object DefaultMaxDistance extends MaxDistanceOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class MaxDistance(n: Integer) extends MaxDistanceOptions {
    def isEmpty = false
    def view = "max_dist" -> values.expr(n) :: Nil
    val innerQuery = query
  }

}
