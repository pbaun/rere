package rere.ql.options

import rere.ql.queries.values

trait MaxResultsOptions {

  sealed trait MaxResultsOptions extends ComposableOptions

  case object DefaultMaxResults extends MaxResultsOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class MaxResults(n: Integer) extends MaxResultsOptions {
    def isEmpty = false
    def view = "max_results" -> values.expr(n) :: Nil
    val innerQuery = query
  }

}
