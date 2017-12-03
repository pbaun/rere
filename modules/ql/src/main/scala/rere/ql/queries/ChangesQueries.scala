package rere.ql.queries

import rere.ql.options.ComposableOptions
import rere.ql.options.all._
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait ChangesQueries {

  // changes
  trait ChangesTableQuery[T] extends ReqlInfiniteStream[ReqlChangefeedNotification[T]]
  trait ChangesTableSliceQuery[T] extends ReqlInfiniteStream[ReqlChangefeedNotification[T]]
  trait ChangesSelectionOfObjectQuery[T] extends ReqlInfiniteStream[ReqlChangefeedNotification[T]]

  implicit class ChangesOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    def changes(
      squash: SquashOptions = NotSquash,
      changefeedQueueSize: ChangefeedQueueSizeOptions = DefaultChangefeedQueueSize,
      includeInitial: IncludeInitialOptions = NotIncludeInitial,
      includeStates: IncludeStatesOptions = NotIncludeStates,
      //includeOffsets: IncludeOffsetsOptions = NotIncludeOffsets, //ReqlQueryLogicError: Cannot include offsets for range subs in:
      includeTypes: IncludeTypesOptions = NotIncludeTypes
    ): ChangesTableQuery[T] = new ChangesTableQuery[T] {
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

  implicit class ChangesOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    def changes(
      squash: SquashOptions = NotSquash,
      changefeedQueueSize: ChangefeedQueueSizeOptions = DefaultChangefeedQueueSize,
      includeInitial: IncludeInitialOptions = NotIncludeInitial,
      includeStates: IncludeStatesOptions = NotIncludeStates,
      //includeOffsets: IncludeOffsetsOptions = NotIncludeOffsets, //ReqlQueryLogicError: Cannot include offsets for range subs in:
      includeTypes: IncludeTypesOptions = NotIncludeTypes
    ): ChangesTableSliceQuery[T] = new ChangesTableSliceQuery[T] {
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

  implicit class ChangesOnSelectionOfObjectOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfObject[T, PK]) {
    def changes(
      squash: SquashOptions = NotSquash,
      changefeedQueueSize: ChangefeedQueueSizeOptions = DefaultChangefeedQueueSize,
      includeInitial: IncludeInitialOptions = NotIncludeInitial,
      includeStates: IncludeStatesOptions = NotIncludeStates,
      //includeOffsets: IncludeOffsetsOptions = NotIncludeOffsets, //ReqlQueryLogicError: Cannot include offsets for range subs in:
      includeTypes: IncludeTypesOptions = NotIncludeTypes
    ): ChangesSelectionOfObjectQuery[T] = new ChangesSelectionOfObjectQuery[T] {
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
