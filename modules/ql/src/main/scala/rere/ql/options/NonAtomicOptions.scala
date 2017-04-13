package rere.ql.options

import rere.ql.queries.values

trait NonAtomicOptions {

  sealed trait NonAtomicOptions extends ComposableOptions

  case object AtomicUpdate extends NonAtomicOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object NonAtomicUpdate extends NonAtomicOptions {
    def isEmpty = false
    def view = "non_atomic" -> values.expr(true) :: Nil
    val expr = exprFromView
  }

}
