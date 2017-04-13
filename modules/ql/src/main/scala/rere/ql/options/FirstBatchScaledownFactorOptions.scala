package rere.ql.options

import rere.ql.queries.values

trait FirstBatchScaledownFactorOptions {

  sealed trait FirstBatchScaledownFactorOptions extends ComposableOptions

  case object DefaultFirstBatchScaledownFactor extends FirstBatchScaledownFactorOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class FirstBatchScaledownFactor(factor: Long) extends FirstBatchScaledownFactorOptions {
    def isEmpty = false
    def view = "first_batch_scaledown_factor" -> values.expr(factor) :: Nil
    def expr = exprFromView
  }

}
