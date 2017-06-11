package rere.ql.options

import rere.ql.queries.values

trait NonAtomicOptions {

  sealed trait NonAtomicOptions extends ComposableOptions

  case object AtomicUpdate extends NonAtomicOptions with DefaultOption

  case object NonAtomicUpdate extends NonAtomicOptions with NonDefaultOption {
    def view = "non_atomic" -> values.expr(true) :: Nil
  }

}
