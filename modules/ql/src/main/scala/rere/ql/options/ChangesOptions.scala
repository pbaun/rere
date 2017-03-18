package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.{ReqlFloat, ReqlInteger}

trait ChangesOptions {

  sealed trait SquashOptions extends ComposableOptions

  case object NotSquash extends SquashOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object DoSquash extends SquashOptions {
    def isEmpty = false
    def view = "squash" -> values.expr(true) :: Nil
    def innerQuery = query
  }

  case class SquashDuring(nSeconds: ReqlFloat) extends SquashOptions {
    def isEmpty = false
    def view = "squash" -> nSeconds :: Nil
    def innerQuery = query
  }


  sealed trait ChangefeedQueueSizeOptions extends ComposableOptions

  case object DefaultChangefeedQueueSize extends ChangefeedQueueSizeOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class ChangefeedQueueSize(size: ReqlInteger) extends ChangefeedQueueSizeOptions {
    def isEmpty = false
    def view = "changefeed_queue_size" -> size :: Nil
    def innerQuery = query
  }


  sealed trait IncludeInitialOptions extends ComposableOptions

  case object NotIncludeInitial extends IncludeInitialOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object IncludeInitial extends IncludeInitialOptions {
    def isEmpty = false
    def view = "include_initial" -> values.expr(true) :: Nil
    def innerQuery = query
  }


  sealed trait IncludeStatesOptions extends ComposableOptions

  case object NotIncludeStates extends IncludeStatesOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object IncludeStates extends IncludeStatesOptions {
    def isEmpty = false
    def view = "include_states" -> values.expr(true) :: Nil
    def innerQuery = query
  }


  sealed trait IncludeOffsetsOptions extends ComposableOptions

  case object NotIncludeOffsets extends IncludeOffsetsOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object IncludeOffsets extends IncludeOffsetsOptions {
    def isEmpty = false
    def view = "include_offsets" -> values.expr(true) :: Nil
    def innerQuery = query
  }


  sealed trait IncludeTypesOptions extends ComposableOptions

  case object NotIncludeTypes extends IncludeTypesOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object IncludeTypes extends IncludeTypesOptions {
    def isEmpty = false
    def view = "include_types" -> values.expr(true) :: Nil
    def innerQuery = query
  }

}
