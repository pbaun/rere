package rere.ql.options

import rere.ql.queries.values

trait FirstBatchScaledownFactorOptions {

  sealed trait FirstBatchScaledownFactorOptions extends ComposableOptions

  case object DefaultFirstBatchScaledownFactor extends FirstBatchScaledownFactorOptions with DefaultOption

  case class FirstBatchScaledownFactor(factor: Long) extends FirstBatchScaledownFactorOptions with NonDefaultOption {
    def view = "first_batch_scaledown_factor" -> values.expr(factor) :: Nil
  }

}
