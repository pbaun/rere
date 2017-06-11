package rere.ql.options

import rere.ql.queries.values

trait InterleaveOptions {

  sealed trait InterleaveOptions extends ComposableOptions

  case object Mix extends InterleaveOptions with DefaultOption

  case object PreventMixing extends InterleaveOptions with NonDefaultOption {
    def view = "interleave" -> values.expr(false) :: Nil
  }

  //TODO: MergeSort with function
  case class MergeSort(fieldName: String) extends InterleaveOptions with NonDefaultOption {
    def view = "interleave" -> values.expr(fieldName) :: Nil
  }

}
