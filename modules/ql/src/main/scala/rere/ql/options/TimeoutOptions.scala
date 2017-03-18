package rere.ql.options

import rere.ql.queries.values

trait TimeoutOptions {

  sealed trait TimeoutOptions extends ComposableOptions

  case object WithoutTimeout extends TimeoutOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class WithTimeout(timeout: Int) extends TimeoutOptions {
    def isEmpty = false
    def view = "timeout" -> values.expr(timeout) :: Nil
    def innerQuery = query
  }

}
