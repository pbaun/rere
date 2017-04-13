package rere.ql.options

import rere.ql.queries.values

trait MaxBatchBytesOptions {

  sealed trait MaxBatchBytesOptions extends ComposableOptions

  case object DefaultMaxBatchBytes extends MaxBatchBytesOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class MaxBatchBytes(bytes: Long) extends MaxBatchBytesOptions {
    def isEmpty = false
    def view = "max_batch_bytes" -> values.expr(bytes) :: Nil
    def expr = exprFromView
  }

}
