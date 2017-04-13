package rere.ql.options

import rere.ql.queries.values

trait MinBatchRowsOptions {

  sealed trait MinBatchRowsOptions extends ComposableOptions

  case object DefaultMinBatchRows extends MinBatchRowsOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class MinBatchRows(n: Long) extends MinBatchRowsOptions {
    def isEmpty = false
    def view = "min_batch_rows" -> values.expr(n) :: Nil
    def expr = exprFromView
  }

}
