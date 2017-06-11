package rere.ql.options

import rere.ql.ql2.Term.TermType
import rere.ql.queries.{control, values}

trait DefaultOptions {
  sealed trait DefaultOptions extends ComposableOptions

  case object Skip extends DefaultOptions with DefaultOption

  case object NoSkip extends DefaultOptions with NonDefaultOption {
    def view = "default" -> values.expr(true) :: Nil
  }

  // special version of r.error without message
  private val rethrowError: control.ErrorQuery = new control.ErrorQuery {
    val command = TermType.ERROR
    val string = "error"
    val arguments = Nil
    val options = Options.empty
  }

  case object RethrowError extends DefaultOptions with NonDefaultOption {
    def view = "default" -> rethrowError :: Nil
  }
}
