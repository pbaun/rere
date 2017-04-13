package rere.ql.options

import rere.ql.ql2.Term.TermType
import rere.ql.queries.{control, values}

trait DefaultOptions {
  sealed trait DefaultOptions extends ComposableOptions

  // default - not need to transmit
  case object Skip extends DefaultOptions {
    def isEmpty = true
    def view = Nil      // "default" -> false
    val expr = exprFromView
  }

  case object NoSkip extends DefaultOptions {
    def isEmpty = false
    def view = "default" -> values.expr(true) :: Nil
    val expr = exprFromView
  }

  // special version of r.error without message
  private val rethrowError: control.ErrorQuery = new control.ErrorQuery {
    val command = TermType.ERROR
    val string = "error"
    val arguments = Nil
    val options = Options.empty
  }

  case object RethrowError extends DefaultOptions {
    def isEmpty = false
    def view = "default" -> rethrowError :: Nil
    val expr = exprFromView
  }
}
