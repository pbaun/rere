package rere.ql.options

import rere.ql.queries.values

trait InterleaveOptions {

  sealed trait InterleaveOptions extends ComposableOptions

  case object Mix extends InterleaveOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object PreventMixing extends InterleaveOptions {
    def isEmpty = false
    def view = "interleave" -> values.expr(false) :: Nil
    val expr = exprFromView
  }

  //TODO: MergeSort with function
  case class MergeSort(fieldName: String) extends InterleaveOptions {
    def isEmpty = false
    def view = "interleave" -> values.expr(fieldName) :: Nil
    def expr = exprFromView
  }

}
