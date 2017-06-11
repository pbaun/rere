package rere.ql.options

import rere.ql.queries.values

trait MinBatchRowsOptions {

  sealed trait MinBatchRowsOptions extends ComposableOptions

  case object DefaultMinBatchRows extends MinBatchRowsOptions with DefaultOption

  case class MinBatchRows(n: Long) extends MinBatchRowsOptions with NonDefaultOption {
    def view = "min_batch_rows" -> values.expr(n) :: Nil
  }

}
