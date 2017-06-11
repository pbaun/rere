package rere.ql.options

import rere.ql.queries.values

trait MaxBatchSecondsOptions {

  sealed trait MaxBatchSecondsOptions extends ComposableOptions

  case object DefaultMaxBatchSeconds extends MaxBatchSecondsOptions with DefaultOption

  case class MaxBatchSeconds(seconds: Double) extends MaxBatchSecondsOptions with NonDefaultOption {
    def view = "max_batch_seconds" -> values.expr(seconds) :: Nil
  }

}
