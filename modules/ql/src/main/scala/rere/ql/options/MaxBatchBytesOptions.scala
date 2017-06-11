package rere.ql.options

import rere.ql.queries.values

trait MaxBatchBytesOptions {

  sealed trait MaxBatchBytesOptions extends ComposableOptions

  case object DefaultMaxBatchBytes extends MaxBatchBytesOptions with DefaultOption

  case class MaxBatchBytes(bytes: Long) extends MaxBatchBytesOptions with NonDefaultOption {
    def view = "max_batch_bytes" -> values.expr(bytes) :: Nil
  }

}
