package rere.ql.queries

import rere.ql.options.ComposableOptions
import rere.ql.options.all._
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait ChangesQueries {

  // changes
  trait ChangesTableQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait ChangesTableSliceQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait ChangesSelectionOfObjectQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]

  implicit class ChangesOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def changes(
      squash: SquashOptions = NotSquash,
      changefeedQueueSize: ChangefeedQueueSizeOptions = DefaultChangefeedQueueSize,
      includeInitial: IncludeInitialOptions = NotIncludeInitial,
      includeStates: IncludeStatesOptions = NotIncludeStates,
      //includeOffsets: IncludeOffsetsOptions = NotIncludeOffsets, //ReqlQueryLogicError: Cannot include offsets for range subs in:
      includeTypes: IncludeTypesOptions = NotIncludeTypes
    ): ChangesTableQuery[ReqlChangefeedNotification[T]] = new ChangesTableQuery[ReqlChangefeedNotification[T]] {
      val command = TermType.CHANGES
      val string = "changes"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(
        squash,
        changefeedQueueSize,
        includeInitial,
        includeStates,
        //includeOffsets,
        includeTypes
      )
    }
  }

  implicit class ChangesOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def changes(
      squash: SquashOptions = NotSquash,
      changefeedQueueSize: ChangefeedQueueSizeOptions = DefaultChangefeedQueueSize,
      includeInitial: IncludeInitialOptions = NotIncludeInitial,
      includeStates: IncludeStatesOptions = NotIncludeStates,
      //includeOffsets: IncludeOffsetsOptions = NotIncludeOffsets, //ReqlQueryLogicError: Cannot include offsets for range subs in:
      includeTypes: IncludeTypesOptions = NotIncludeTypes
    ): ChangesTableSliceQuery[ReqlChangefeedNotification[T]] = new ChangesTableSliceQuery[ReqlChangefeedNotification[T]] {
      val command = TermType.CHANGES
      val string = "changes"
      val arguments = tableSlice :: Nil
      val options = ComposableOptions.compose(
        squash,
        changefeedQueueSize,
        includeInitial,
        includeStates,
        //includeOffsets,
        includeTypes
      )
    }
  }

  implicit class ChangesOnSelectionOfObjectOp[T <: ReqlObject](val sel: ReqlSelectionOfObject[T]) {
    def changes(
      squash: SquashOptions = NotSquash,
      changefeedQueueSize: ChangefeedQueueSizeOptions = DefaultChangefeedQueueSize,
      includeInitial: IncludeInitialOptions = NotIncludeInitial,
      includeStates: IncludeStatesOptions = NotIncludeStates,
      //includeOffsets: IncludeOffsetsOptions = NotIncludeOffsets, //ReqlQueryLogicError: Cannot include offsets for range subs in:
      includeTypes: IncludeTypesOptions = NotIncludeTypes
    ): ChangesSelectionOfObjectQuery[ReqlChangefeedNotification[T]] = new ChangesSelectionOfObjectQuery[ReqlChangefeedNotification[T]] {
      val command = TermType.CHANGES
      val string = "changes"
      val arguments = sel :: Nil
      val options = ComposableOptions.compose(
        squash,
        changefeedQueueSize,
        includeInitial,
        includeStates,
        //includeOffsets,
        includeTypes
      )
    }
  }

}
