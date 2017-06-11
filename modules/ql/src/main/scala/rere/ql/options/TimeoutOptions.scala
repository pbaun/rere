package rere.ql.options

import rere.ql.queries.values

trait TimeoutOptions {

  sealed trait TimeoutOptions extends ComposableOptions

  case object WithoutTimeout extends TimeoutOptions with DefaultOption

  case class WithTimeout(timeout: Int) extends TimeoutOptions with NonDefaultOption {
    def view = "timeout" -> values.expr(timeout) :: Nil
  }

}
