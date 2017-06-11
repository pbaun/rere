package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.{ReqlFloat, ReqlInteger}

trait ChangesOptions {

  sealed trait SquashOptions extends ComposableOptions

  case object NotSquash extends SquashOptions with DefaultOption

  case object DoSquash extends SquashOptions with NonDefaultOption {
    def view = "squash" -> values.expr(true) :: Nil
  }

  case class SquashDuring(nSeconds: ReqlFloat) extends SquashOptions with NonDefaultOption {
    def view = "squash" -> nSeconds :: Nil
  }


  sealed trait ChangefeedQueueSizeOptions extends ComposableOptions

  case object DefaultChangefeedQueueSize extends ChangefeedQueueSizeOptions with DefaultOption

  case class ChangefeedQueueSize(size: ReqlInteger) extends ChangefeedQueueSizeOptions with NonDefaultOption {
    def view = "changefeed_queue_size" -> size :: Nil
  }


  sealed trait IncludeInitialOptions extends ComposableOptions

  case object NotIncludeInitial extends IncludeInitialOptions with DefaultOption

  case object IncludeInitial extends IncludeInitialOptions with NonDefaultOption {
    def view = "include_initial" -> values.expr(true) :: Nil
  }


  sealed trait IncludeStatesOptions extends ComposableOptions

  case object NotIncludeStates extends IncludeStatesOptions with DefaultOption

  case object IncludeStates extends IncludeStatesOptions with NonDefaultOption {
    def view = "include_states" -> values.expr(true) :: Nil
  }


  sealed trait IncludeOffsetsOptions extends ComposableOptions

  case object NotIncludeOffsets extends IncludeOffsetsOptions with DefaultOption

  case object IncludeOffsets extends IncludeOffsetsOptions with NonDefaultOption {
    def view = "include_offsets" -> values.expr(true) :: Nil
  }


  sealed trait IncludeTypesOptions extends ComposableOptions

  case object NotIncludeTypes extends IncludeTypesOptions with DefaultOption

  case object IncludeTypes extends IncludeTypesOptions with NonDefaultOption {
    def view = "include_types" -> values.expr(true) :: Nil
  }

}
