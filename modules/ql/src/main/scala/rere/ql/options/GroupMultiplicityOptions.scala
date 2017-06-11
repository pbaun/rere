package rere.ql.options

import rere.ql.queries.values

trait GroupMultiplicityOptions {

  sealed trait GroupMultiplicityOptions extends ComposableOptions

  case object DefaultGroupMultiplicity extends GroupMultiplicityOptions with DefaultOption

  case object MultiGroup extends GroupMultiplicityOptions with NonDefaultOption {
    def view = "multi" -> values.expr(true) :: Nil
  }

}
