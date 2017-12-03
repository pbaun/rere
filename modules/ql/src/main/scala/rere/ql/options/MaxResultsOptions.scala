package rere.ql.options

import rere.ql.queries.values

trait MaxResultsOptions {

  sealed trait MaxResultsOptions extends ComposableOptions

  case object DefaultMaxResults extends MaxResultsOptions with DefaultOption

  case class MaxResults(n: Int) extends MaxResultsOptions with NonDefaultOption {
    def view = "max_results" -> values.expr(n) :: Nil
  }

}
