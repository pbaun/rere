package rere.ql.options

import rere.ql.queries.values

trait MaxBatchRowsOptions {

  sealed trait MaxBatchRowsOptions extends ComposableOptions

  case object DefaultMaxBatchRows extends MaxBatchRowsOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class MaxBatchRows(n: Long) extends MaxBatchRowsOptions {
    def isEmpty = false
    def view = "max_batch_rows" -> values.expr(n) :: Nil
    def expr = exprFromView
  }

}
