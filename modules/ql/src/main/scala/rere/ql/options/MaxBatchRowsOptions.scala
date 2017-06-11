package rere.ql.options

import rere.ql.queries.values

trait MaxBatchRowsOptions {

  sealed trait MaxBatchRowsOptions extends ComposableOptions

  case object DefaultMaxBatchRows extends MaxBatchRowsOptions with DefaultOption

  case class MaxBatchRows(n: Long) extends MaxBatchRowsOptions with NonDefaultOption {
    def view = "max_batch_rows" -> values.expr(n) :: Nil
  }

}
