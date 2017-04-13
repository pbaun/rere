package rere.ql.options

import rere.ql.queries.values

trait MaxBatchSecondsOptions {

  sealed trait MaxBatchSecondsOptions extends ComposableOptions

  case object DefaultMaxBatchSeconds extends MaxBatchSecondsOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class MaxBatchSeconds(seconds: Double) extends MaxBatchSecondsOptions {
    def isEmpty = false
    def view = "max_batch_seconds" -> values.expr(seconds) :: Nil
    def expr = exprFromView
  }

}
