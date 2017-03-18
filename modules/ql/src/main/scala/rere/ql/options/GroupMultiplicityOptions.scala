package rere.ql.options

import rere.ql.queries.values

trait GroupMultiplicityOptions {

  sealed trait GroupMultiplicityOptions extends ComposableOptions

  case object DefaultGroupMultiplicity extends GroupMultiplicityOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object MultiGroup extends GroupMultiplicityOptions {
    def isEmpty = false
    def view = "multi" -> values.expr(true) :: Nil
    val innerQuery = query
  }

}
