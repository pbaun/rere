package rere.ql.queries

import rere.ql.options.all._
import rere.ql.options.{ComposableOptions, Options}
import rere.ql.ql2.Term.TermType
import rere.ql.typeclasses.{DatumSelector, ToPredicate, Transmuter}
import rere.ql.types._

trait AggregationQueries {

  // group
  //TODO: rethink all that
  trait GroupTableQuery[K, R] extends ReqlGroupedStream[K, R]

  implicit class GroupOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    //it returns immediately like array
    def group[U <: ReqlDatum](
      selector: DatumSelector[ReqlModel[T, PK], U]
    ): GroupTableQuery[U, ReqlModel[T, PK]] = new GroupTableQuery[U, ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selector :: Nil
      val options = Options.empty
    }

    def group[U <: ReqlDatum](
      selector0: DatumSelector[ReqlModel[T, PK], U],
      selector1: DatumSelector[ReqlModel[T, PK], U]
    ): GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] = new GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selector0 :: selector1 :: Nil
      val options = Options.empty
    }

    /*def group[U <: ReqlDatum](selectors: ReqlDatumSelector2[T, U]*): GroupTableQuery = new GroupTableQuery {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selectors.toList
      val options = Options.empty
    }*/

    //TODO: find way to pass index after selectors - it like db will combine key (selectors: _*, index)
    def group[U <: ReqlDatum](
      index: IndexOptions
    ): GroupTableQuery[U, ReqlModel[T, PK]] = new GroupTableQuery[U, ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: Nil
      val options = index
    }

    def group[U <: ReqlDatum](
      selector: DatumSelector[ReqlModel[T, PK], U],
      index: IndexOptions
    ): GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] = new GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selector :: Nil
      val options = index
    }

    def group[U <: ReqlDatum](
      selector0: DatumSelector[ReqlModel[T, PK], U],
      selector1: DatumSelector[ReqlModel[T, PK], U],
      index: IndexOptions
    ): GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] = new GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selector0 :: selector1 :: Nil
      val options = index
    }

    /*def group[U <: ReqlDatum](index: IndexOptions, selectors: ReqlDatumSelector2[T, U]*): GroupTableQuery = new GroupTableQuery {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selectors.toList
      val options = index
    }*/

    def group[U <: ReqlDatum](
      selector: DatumSelector[ReqlModel[T, PK], U],
      multi: GroupMultiplicityOptions
    ): GroupTableQuery[U, ReqlModel[T, PK]] = new GroupTableQuery[U, ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selector :: Nil
      val options = multi
    }

    def group[U <: ReqlDatum](
      selector0: DatumSelector[ReqlModel[T, PK], U],
      selector1: DatumSelector[ReqlModel[T, PK], U],
      multi: GroupMultiplicityOptions
    ): GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] = new GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selector0 :: selector1 :: Nil
      val options = multi
    }

    /*def group[U <: ReqlDatum](multi: GroupMultiplicityOptions, selectors: ReqlDatumSelector2[T, U]*): GroupTableQuery = new GroupTableQuery {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selectors.toList
      val options = multi
    }*/

    def group[U <: ReqlDatum](
      index: IndexOptions,
      multi: GroupMultiplicityOptions
    ): GroupTableQuery[U, ReqlModel[T, PK]] = new GroupTableQuery[U, ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(index, multi)
    }

    def group[U <: ReqlDatum](
      selector: DatumSelector[ReqlModel[T, PK], U],
      index: IndexOptions,
      multi: GroupMultiplicityOptions
    ): GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] = new GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selector :: Nil
      val options = ComposableOptions.compose(index, multi)
    }

    def group[U <: ReqlDatum](
      selector0: DatumSelector[ReqlModel[T, PK], U],
      selector1: DatumSelector[ReqlModel[T, PK], U],
      index: IndexOptions,
      multi: GroupMultiplicityOptions
    ): GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] = new GroupTableQuery[ReqlArray[U], ReqlModel[T, PK]] {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selector0 :: selector1 :: Nil
      val options = ComposableOptions.compose(index, multi)
    }

    /*def group[U <: ReqlDatum](multi: GroupMultiplicityOptions, index: IndexOptions, selectors: ReqlDatumSelector2[T, U]*): GroupTableQuery = new GroupTableQuery {
      val command = TermType.GROUP
      val string = "group"
      val arguments = table :: selectors.toList
      val options = ComposableOptions.compose(index, multi)
    }*/
  }

  // ungroup
  trait UngroupGroupedStreamQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait UngroupGroupedDataQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class UngroupOnGroupedStreamOp[K, T <: ReqlDatum](val groupedStream: ReqlGroupedStream[K, T]) {
    def ungroup(): UngroupGroupedStreamQuery[T] = new UngroupGroupedStreamQuery[T] {
      val command = TermType.UNGROUP
      val string = "ungroup"
      val arguments = groupedStream :: Nil
      val options = Options.empty
    }
  }

  implicit class UngroupOnGroupedDataOp[T <: ReqlDatum](val groupedData: ReqlGroupedData) {
    def ungroup(): UngroupGroupedDataQuery[T] = new UngroupGroupedDataQuery[T] {
      val command = TermType.UNGROUP
      val string = "ungroup"
      val arguments = groupedData :: Nil
      val options = Options.empty
    }
  }

  // reduce
  trait ReduceTableQuery extends ReqlDatum
  trait ReduceTableSliceQuery extends ReqlDatum
  trait ReduceSelectionOfArrayQuery extends ReqlDatum
  trait ReduceSelectionOfStreamQuery extends ReqlDatum
  trait ReduceArrayQuery extends ReqlDatum

  implicit class ReduceOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    private implicit def modelShape = table.shape

    def reduce(
      reductionFunction: (ReqlModel[T, PK], ReqlModel[T, PK]) => ReqlModel[T, PK]
    ): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new ReduceTableQuery {
          val command = TermType.REDUCE
          val string = "reduce"
          val arguments = table :: Func.wrap2(reductionFunction) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class ReduceOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    private implicit def modelShape = tableSlice.shape

    def reduce(
      reductionFunction: (ReqlModel[T, PK], ReqlModel[T, PK]) => ReqlModel[T, PK]
    ): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new ReduceTableSliceQuery {
          val command = TermType.REDUCE
          val string = "reduce"
          val arguments = tableSlice :: Func.wrap2(reductionFunction) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class ReduceOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    private implicit def modelShape = sel.shape

    def reduce(
      reductionFunction: (ReqlModel[T, PK], ReqlModel[T, PK]) => ReqlModel[T, PK]
    ): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new ReduceSelectionOfArrayQuery {
          val command = TermType.REDUCE
          val string = "reduce"
          val arguments = sel :: Func.wrap2(reductionFunction) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class ReduceOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    private implicit def modelShape = sel.shape

    def reduce(
      reductionFunction: (ReqlModel[T, PK], ReqlModel[T, PK]) => ReqlModel[T, PK]
    ): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new ReduceSelectionOfStreamQuery {
          val command = TermType.REDUCE
          val string = "reduce"
          val arguments = sel :: Func.wrap2(reductionFunction) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class ReduceOnArrayOp[T <: ReqlDatum : Transmuter](val array: ReqlArray[T]) {
    def reduce(
      reductionFunction: (T, T) => T
    ): T = {
      Transmuter.transmute[T](
        new ReduceArrayQuery {
          val command = TermType.REDUCE
          val string = "reduce"
          val arguments = array :: Func.wrap2(reductionFunction) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  // fold
  trait FoldToValueTableQuery extends ReqlDatum
  trait FoldToSeqTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]

  trait FoldToValueTableSliceQuery extends ReqlDatum
  trait FoldToSeqTableSliceQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]

  trait FoldToValueSelectionOfArrayQuery extends ReqlDatum
  trait FoldToSeqSelectionOfArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  trait FoldToValueSelectionOfStreamQuery extends ReqlDatum
  trait FoldToSeqSelectionOfStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]

  trait FoldToValueFiniteStreamQuery extends ReqlDatum
  trait FoldToSeqFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]

  trait FoldToSeqInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]

  trait FoldToValueArrayQuery extends ReqlDatum
  trait FoldToSeqArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  //TODO: find way to make .fold[R]: R not .fold[R]: Datum
  implicit class FoldOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    private implicit def modelShape = table.shape

    def fold[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, ReqlModel[T, PK]) => R
    ): R = {
      Transmuter.transmute[R](
        new FoldToValueTableQuery {
          val command = TermType.FOLD
          val string = "fold"
          val arguments = table :: base :: Func.wrap2(combiningFunction) :: Nil
          val options = Options.empty
        }
      )
    }

    def foldAndEmit[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, ReqlModel[T, PK]) => R,
      emitOptions: EmitOptions[R, ReqlModel[T, PK]]
    ): FoldToSeqTableQuery[R] = new FoldToSeqTableQuery[R] {
      val command = TermType.FOLD
      val string = "fold"
      val arguments = table :: base :: Func.wrap2(combiningFunction) :: Nil
      val options = emitOptions
    }
  }

  implicit class FoldOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    private implicit def modelShape = tableSlice.shape

    def fold[
      R <: ReqlDatum : Transmuter
    ](
      base: R)(
      combiningFunction: (R, ReqlModel[T, PK]) => R
    ): R = {
      Transmuter.transmute[R](
        new FoldToValueTableSliceQuery {
          val command = TermType.FOLD
          val string = "fold"
          val arguments = tableSlice :: base :: Func.wrap2(combiningFunction) :: Nil
          val options = Options.empty
        }
      )
    }

    def foldAndEmit[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, ReqlModel[T, PK]) => R,
      emitOptions: EmitOptions[R, ReqlModel[T, PK]]
    ): FoldToSeqTableSliceQuery[R] = new FoldToSeqTableSliceQuery[R] {
      val command = TermType.FOLD
      val string = "fold"
      val arguments = tableSlice :: base :: Func.wrap2(combiningFunction) :: Nil
      val options = emitOptions
    }
  }

  implicit class FoldOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    private implicit def modelShape = sel.shape

    def fold[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, ReqlModel[T, PK]) => R
    ): R = {
      Transmuter.transmute[R](
        new FoldToValueSelectionOfArrayQuery {
          val command = TermType.FOLD
          val string = "fold"
          val arguments = sel :: base :: Func.wrap2(combiningFunction) :: Nil
          val options = Options.empty
        }
      )
    }

    def foldAndEmit[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, ReqlModel[T, PK]) => R,
      emitOptions: EmitOptions[R, ReqlModel[T, PK]]
    ): FoldToSeqSelectionOfArrayQuery[R] = new FoldToSeqSelectionOfArrayQuery[R] {
      val command = TermType.FOLD
      val string = "fold"
      val arguments = sel :: base :: Func.wrap2(combiningFunction) :: Nil
      val options = emitOptions
    }
  }

  implicit class FoldOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    private implicit def modelShape = sel.shape

    def fold[
      R <: ReqlDatum : Transmuter
    ](
      base: R)(
      combiningFunction: (R, ReqlModel[T, PK]) => R
    ): R = {
      Transmuter.transmute[R](
        new FoldToValueSelectionOfStreamQuery {
          val command = TermType.FOLD
          val string = "fold"
          val arguments = sel :: base :: Func.wrap2(combiningFunction) :: Nil
          val options = Options.empty
        }
      )
    }

    def foldAndEmit[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, ReqlModel[T, PK]) => R,
      emitOptions: EmitOptions[R, ReqlModel[T, PK]]
    ): FoldToSeqSelectionOfStreamQuery[R] = new FoldToSeqSelectionOfStreamQuery[R] {
      val command = TermType.FOLD
      val string = "fold"
      val arguments = sel :: base :: Func.wrap2(combiningFunction) :: Nil
      val options = emitOptions
    }
  }

  implicit class FoldOnFiniteStreamOp[T <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T]) {
    def fold[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, T) => R
    ): FoldToValueFiniteStreamQuery = new FoldToValueFiniteStreamQuery {
      val command = TermType.FOLD
      val string = "fold"
      val arguments = finiteStream :: base :: Func.wrap2(combiningFunction) :: Nil
      val options = Options.empty
    }

    def foldAndEmit[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, T) => R,
      emitOptions: EmitOptions[R, ReqlDatum]
    ): FoldToSeqFiniteStreamQuery[R] = new FoldToSeqFiniteStreamQuery[R] {
      val command = TermType.FOLD
      val string = "fold"
      val arguments = finiteStream :: base :: Func.wrap2(combiningFunction) :: Nil
      val options = emitOptions
    }
  }

  implicit class FoldOnInfiniteStreamOp[T <: ReqlDatum : Transmuter](val infiniteStream: ReqlInfiniteStream[T]) {
    def foldAndEmit[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, T) => R,
      emitOptions: EmitOptions[R, T]
    ): FoldToSeqInfiniteStreamQuery[T] = new FoldToSeqInfiniteStreamQuery[T] {
      val command = TermType.FOLD
      val string = "fold"
      val arguments = infiniteStream :: base :: Func.wrap2(combiningFunction) :: Nil
      val options = emitOptions
    }
  }

  implicit class FoldOnArrayOp[T <: ReqlDatum : Transmuter](val array: ReqlArray[T]) {
    def fold[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, T) => R
    ): FoldToValueArrayQuery = new FoldToValueArrayQuery {
      val command = TermType.FOLD
      val string = "fold"
      val arguments = array :: base :: Func.wrap2(combiningFunction) :: Nil
      val options = Options.empty
    }

    def foldAndEmit[
      R <: ReqlDatum : Transmuter
    ](
      base: R
    )(
      combiningFunction: (R, T) => R,
      emitOptions: EmitOptions[R, ReqlDatum]
    ): FoldToSeqArrayQuery[R] = new FoldToSeqArrayQuery[R] {
      val command = TermType.FOLD
      val string = "fold"
      val arguments = array :: base :: Func.wrap2(combiningFunction) :: Nil
      val options = emitOptions
    }
  }

  // count
  trait CountTableQuery extends ReqlInteger
  trait CountTableSliceQuery extends ReqlInteger
  trait CountSelectionOfArrayQuery extends ReqlInteger
  trait CountSelectionOfStreamQuery extends ReqlInteger
  trait CountFiniteStreamQuery extends ReqlInteger
  trait CountArrayQuery extends ReqlInteger
  trait CountBinaryQuery extends ReqlInteger
  trait CountStringQuery extends ReqlInteger
  trait CountObjectQuery extends ReqlInteger
  trait CountGroupedStreamQuery extends ReqlGroupedData

  implicit class CountOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    private implicit def modelShape = table.shape

    def count(): CountTableQuery = new CountTableQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = table :: Nil
      val options = Options.empty
    }

    def count(datum: ReqlModel[T, PK]): CountTableQuery = new CountTableQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = table :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def count(function: ReqlModel[T, PK] => ReqlBoolean): CountTableQuery = new CountTableQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = table :: ToPredicate(function) :: Nil
      val options = Options.empty
    }

    def count(predicate: ReqlPredicate[ReqlModel[T, PK]]): CountTableQuery = new CountTableQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = table :: predicate :: Nil
      val options = Options.empty
    }
  }

  implicit class CountOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    private implicit def modelShape = tableSlice.shape

    def count(): CountTableSliceQuery = new CountTableSliceQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = tableSlice :: Nil
      val options = Options.empty
    }

    def count(datum: ReqlModel[T, PK]): CountTableSliceQuery = new CountTableSliceQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = tableSlice :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def count(function: ReqlModel[T, PK] => ReqlBoolean): CountTableSliceQuery = new CountTableSliceQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = tableSlice :: ToPredicate(function) :: Nil
      val options = Options.empty
    }

    def count(predicate: ReqlPredicate[ReqlModel[T, PK]]): CountTableSliceQuery = new CountTableSliceQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = tableSlice :: predicate :: Nil
      val options = Options.empty
    }
  }

  implicit class CountOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    private implicit def modelShape = sel.shape

    def count(): CountSelectionOfArrayQuery = new CountSelectionOfArrayQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = sel :: Nil
      val options = Options.empty
    }

    def count(datum: ReqlModel[T, PK]): CountSelectionOfArrayQuery = new CountSelectionOfArrayQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = sel :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def count(function: ReqlModel[T, PK] => ReqlBoolean): CountSelectionOfArrayQuery = new CountSelectionOfArrayQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = sel :: ToPredicate(function) :: Nil
      val options = Options.empty
    }

    def count(predicate: ReqlPredicate[ReqlModel[T, PK]]): CountSelectionOfArrayQuery = new CountSelectionOfArrayQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = sel :: predicate :: Nil
      val options = Options.empty
    }
  }

  implicit class CountOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    private implicit def modelShape = sel.shape

    def count(): CountSelectionOfStreamQuery = new CountSelectionOfStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = sel :: Nil
      val options = Options.empty
    }

    def count(datum: ReqlModel[T, PK]): CountSelectionOfStreamQuery = new CountSelectionOfStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = sel :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def count(function: ReqlModel[T, PK] => ReqlBoolean): CountSelectionOfStreamQuery = new CountSelectionOfStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = sel :: ToPredicate(function) :: Nil
      val options = Options.empty
    }

    def count(predicate: ReqlPredicate[ReqlModel[T, PK]]): CountSelectionOfStreamQuery = new CountSelectionOfStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = sel :: predicate :: Nil
      val options = Options.empty
    }
  }

  implicit class CountOnFiniteStreamOp[T <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T]) {
    def count(): CountFiniteStreamQuery = new CountFiniteStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = finiteStream :: Nil
      val options = Options.empty
    }

    def count(datum: T): CountFiniteStreamQuery = new CountFiniteStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = finiteStream :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def count(function: T => ReqlBoolean): CountFiniteStreamQuery = new CountFiniteStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = finiteStream :: ToPredicate(function) :: Nil
      val options = Options.empty
    }

    def count(predicate: ReqlPredicate[T]): CountFiniteStreamQuery = new CountFiniteStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = finiteStream :: predicate :: Nil
      val options = Options.empty
    }
  }

  implicit class CountOnArrayOp[T <: ReqlDatum : Transmuter](val array: ReqlArray[T]) {
    def count(): CountArrayQuery = new CountArrayQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = array :: Nil
      val options = Options.empty
    }

    def count(datum: T): CountArrayQuery = new CountArrayQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = array :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def count(function: T => ReqlBoolean): CountArrayQuery = new CountArrayQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = array :: ToPredicate(function) :: Nil
      val options = Options.empty
    }

    def count(predicate: ReqlPredicate[T]): CountArrayQuery = new CountArrayQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = array :: predicate :: Nil
      val options = Options.empty
    }
  }

  implicit class CountOnBinaryOp(val binary: ReqlBinary) {
    def count(): CountBinaryQuery = new CountBinaryQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = binary :: Nil
      val options = Options.empty
    }
  }

  implicit class CountOnStringOp(val str: ReqlString) {
    def count(): CountStringQuery = new CountStringQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = str :: Nil
      val options = Options.empty
    }
  }

  implicit class CountOnObjectOp(val obj: ReqlObject) {
    def count(): CountObjectQuery = new CountObjectQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = obj :: Nil
      val options = Options.empty
    }
  }

  implicit class CountOnGroupedStreamOp[K, T <: ReqlDatum : Transmuter](val groupedStream: ReqlGroupedStream[K, T]) {
    def count(): CountGroupedStreamQuery = new CountGroupedStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = groupedStream :: Nil
      val options = Options.empty
    }

    def count(datum: T): CountGroupedStreamQuery = new CountGroupedStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = groupedStream :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def count(function: T => ReqlBoolean): CountGroupedStreamQuery = new CountGroupedStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = groupedStream :: ToPredicate(function) :: Nil
      val options = Options.empty
    }

    def count(predicate: ReqlPredicate[T]): CountGroupedStreamQuery = new CountGroupedStreamQuery {
      val command = TermType.COUNT
      val string = "count"
      val arguments = groupedStream :: predicate :: Nil
      val options = Options.empty
    }
  }

  // sum
  //TODO: implement r.sum(...) syntax
  trait SumTableQuery extends ReqlFloat
  trait SubTableSliceQuery extends ReqlFloat
  trait SumSelectionOfArrayQuery extends ReqlFloat
  trait SumSelectionOfStreamQuery extends ReqlFloat
  trait SumFiniteStreamQuery extends ReqlFloat
  trait SumArrayQuery extends ReqlFloat

  implicit class SumOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    private implicit def modelShape = table.shape

    def sum(field: ReqlString): SumTableQuery = new SumTableQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = table :: field :: Nil
      val options = Options.empty
    }

    def sum(fun: ReqlModel[T, PK] => ReqlNumber): SumTableQuery = new SumTableQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = table :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class SumOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    private implicit def modelShape = tableSlice.shape

    def sum(field: ReqlString): SubTableSliceQuery = new SubTableSliceQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = tableSlice :: field :: Nil
      val options = Options.empty
    }

    def sum(fun: ReqlModel[T, PK] => ReqlNumber): SubTableSliceQuery = new SubTableSliceQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = tableSlice :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class SumOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    private implicit def modelShape = sel.shape

    def sum(field: ReqlString): SumSelectionOfArrayQuery = new SumSelectionOfArrayQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = sel :: field :: Nil
      val options = Options.empty
    }

    def sum(fun: ReqlModel[T, PK] => ReqlNumber): SumSelectionOfArrayQuery = new SumSelectionOfArrayQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = sel :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class SumOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    private implicit def modelShape = sel.shape

    def sum(field: ReqlString): SumSelectionOfStreamQuery = new SumSelectionOfStreamQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = sel :: field :: Nil
      val options = Options.empty
    }

    def sum(fun: ReqlModel[T, PK] => ReqlNumber): SumSelectionOfStreamQuery = new SumSelectionOfStreamQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = sel :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class SumOnFiniteStreamOp[T <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T]) {
    def sum(): SumFiniteStreamQuery = new SumFiniteStreamQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = finiteStream :: Nil
      val options = Options.empty
    }

    def sum(field: ReqlString): SumFiniteStreamQuery = new SumFiniteStreamQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = finiteStream :: field :: Nil
      val options = Options.empty
    }

    def sum(fun: T => ReqlNumber): SumFiniteStreamQuery = new SumFiniteStreamQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = finiteStream :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class SumOnArrayOp[T <: ReqlDatum : Transmuter](val array: ReqlArray[T]) {
    def sum()(implicit ev: T <:< ReqlNumber): SumArrayQuery = new SumArrayQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = array :: Nil
      val options = Options.empty
    }

    def sum(field: ReqlString): SumArrayQuery = new SumArrayQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = array :: field :: Nil
      val options = Options.empty
    }

    def sum(fun: T => ReqlNumber): SumArrayQuery = new SumArrayQuery {
      val command = TermType.SUM
      val string = "sum"
      val arguments = array :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  // avg
  //TODO: implement r.avg(...) syntax
  trait AvgTableQuery extends ReqlFloat
  trait AvgTableSliceQuery extends ReqlFloat
  trait AvgSelectionOfArrayQuery extends ReqlFloat
  trait AvgSelectionOfStreamQuery extends ReqlFloat
  trait AvgFiniteStreamQuery extends ReqlFloat
  trait AvgArrayQuery extends ReqlFloat

  implicit class AvgOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    private implicit def modelShape = table.shape

    def avg(field: ReqlString): AvgTableQuery = new AvgTableQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = table :: field :: Nil
      val options = Options.empty
    }

    def avg(fun: ReqlModel[T, PK] => ReqlNumber): AvgTableQuery = new AvgTableQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = table :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class AvgOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    private implicit def modelShape = tableSlice.shape

    def avg(field: ReqlString): AvgTableSliceQuery = new AvgTableSliceQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = tableSlice :: field :: Nil
      val options = Options.empty
    }

    def avg(fun: ReqlModel[T, PK] => ReqlNumber): AvgTableSliceQuery = new AvgTableSliceQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = tableSlice :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class AvgOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    private implicit def modelShape = sel.shape

    def avg(field: ReqlString): AvgSelectionOfArrayQuery = new AvgSelectionOfArrayQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = sel :: field :: Nil
      val options = Options.empty
    }

    def avg(fun: ReqlModel[T, PK] => ReqlNumber): AvgSelectionOfArrayQuery = new AvgSelectionOfArrayQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = sel :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class AvgOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    private implicit def modelShape = sel.shape

    def avg(field: ReqlString): AvgSelectionOfStreamQuery = new AvgSelectionOfStreamQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = sel :: field :: Nil
      val options = Options.empty
    }

    def avg(fun: ReqlModel[T, PK] => ReqlNumber): AvgSelectionOfStreamQuery = new AvgSelectionOfStreamQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = sel :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class AvgOnFiniteStreamOp[T <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T]) {
    def avg(): AvgFiniteStreamQuery = new AvgFiniteStreamQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = finiteStream :: Nil
      val options = Options.empty
    }

    def avg(field: ReqlString): AvgFiniteStreamQuery = new AvgFiniteStreamQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = finiteStream :: field :: Nil
      val options = Options.empty
    }

    def avg(fun: T => ReqlNumber): AvgFiniteStreamQuery = new AvgFiniteStreamQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = finiteStream :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  implicit class AvgOnArrayOp[T <: ReqlValue : Transmuter](val array: ReqlArray[T]) {
    def avg(): AvgArrayQuery = new AvgArrayQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = array :: Nil
      val options = Options.empty
    }

    def avg(field: ReqlString): AvgArrayQuery = new AvgArrayQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = array :: field :: Nil
      val options = Options.empty
    }

    def avg(fun: T => ReqlNumber): AvgArrayQuery = new AvgArrayQuery {
      val command = TermType.AVG
      val string = "avg"
      val arguments = array :: Func.wrap1(fun) :: Nil
      val options = Options.empty
    }
  }

  // min
  //TODO: implement r.min(...) syntax
  trait MinTableQuery extends ReqlObject
  trait MinTableSliceQuery extends ReqlObject
  trait MinSelectionOfArrayQuery extends ReqlObject
  trait MinSelectionOfStreamQuery extends ReqlObject
  trait MinFiniteStreamQuery extends ReqlDatum
  trait MinArrayQuery extends ReqlDatum

  implicit class MinOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    private implicit def modelShape = table.shape

    def min(index: IndexOptions = DefaultIndex): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinTableQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = table :: Nil
          val options = index
        }
      )
    }

    def min(field: ReqlString): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinTableQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = table :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def min(fun: ReqlModel[T, PK] => ReqlDatum): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinTableQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = table :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MinOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    private implicit def modelShape = tableSlice.shape

    def min(): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinTableSliceQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = tableSlice :: Nil
          val options = Options.empty
        }
      )
    }

    def min(field: ReqlString): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinTableSliceQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = tableSlice :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def min(fun: ReqlModel[T, PK] => ReqlDatum): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinTableSliceQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = tableSlice :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MinOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    private implicit def modelShape = sel.shape

    def min(): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinSelectionOfArrayQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = sel :: Nil
          val options = Options.empty
        }
      )
    }

    def min(field: ReqlString): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinSelectionOfArrayQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = sel :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def min(fun: ReqlModel[T, PK] => ReqlDatum): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinSelectionOfArrayQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = sel :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MinOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    private implicit def modelShape = sel.shape

    def min(): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinSelectionOfStreamQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = sel :: Nil
          val options = Options.empty
        }
      )
    }

    def min(field: ReqlString): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinSelectionOfStreamQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = sel :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def min(fun: ReqlModel[T, PK] => ReqlDatum): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MinSelectionOfStreamQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = sel :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MinOnFiniteStreamOp[T <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T]) {
    def min(): T = {
      Transmuter.transmute[T](
        new MinFiniteStreamQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = finiteStream :: Nil
          val options = Options.empty
        }
      )
    }

    def min(field: ReqlString): T = {
      Transmuter.transmute[T](
        new MinFiniteStreamQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = finiteStream :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def min(fun: T => ReqlDatum): T = {
      Transmuter.transmute[T](
        new MinFiniteStreamQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = finiteStream :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MinOnArrayOp[T <: ReqlDatum : Transmuter](val array: ReqlArray[T]) {
    def min()(implicit ev: T <:< ReqlNumber): T = {
      Transmuter.transmute[T](
        new MinArrayQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = array :: Nil
          val options = Options.empty
        }
      )
    }

    def min(field: ReqlString): T = {
      Transmuter.transmute[T](
        new MinArrayQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = array :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def min(fun: T => ReqlDatum): T = {
      Transmuter.transmute[T](
        new MinArrayQuery {
          val command = TermType.MIN
          val string = "min"
          val arguments = array :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  // max
  //TODO: implement r.max(...) syntax
  trait MaxTableQuery extends ReqlObject
  trait MaxTableSliceQuery extends ReqlObject
  trait MaxSelectionOfArrayQuery extends ReqlObject
  trait MaxSelectionOfStreamQuery extends ReqlObject
  trait MaxFiniteStreamQuery extends ReqlDatum
  trait MaxArrayQuery extends ReqlDatum

  implicit class MaxOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    private implicit def modelShape = table.shape

    def max(index: IndexOptions = DefaultIndex): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxTableQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = table :: Nil
          val options = index
        }
      )
    }

    def max(field: ReqlString): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxTableQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = table :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def max(fun: ReqlModel[T, PK] => ReqlDatum): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxTableQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = table :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MaxOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    private implicit def modelShape = tableSlice.shape

    def max(): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxTableSliceQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = tableSlice :: Nil
          val options = Options.empty
        }
      )
    }

    def max(field: ReqlString): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxTableSliceQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = tableSlice :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def max(fun: ReqlModel[T, PK] => ReqlDatum): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxTableSliceQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = tableSlice :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MaxOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    private implicit def modelShape = sel.shape

    def max(): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxSelectionOfArrayQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = sel :: Nil
          val options = Options.empty
        }
      )
    }

    def max(field: ReqlString): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxSelectionOfArrayQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = sel :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def max(fun: ReqlModel[T, PK] => ReqlDatum): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxSelectionOfArrayQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = sel :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MaxOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    private implicit def modelShape = sel.shape

    def max(): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxSelectionOfStreamQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = sel :: Nil
          val options = Options.empty
        }
      )
    }

    def max(field: ReqlString): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxSelectionOfStreamQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = sel :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def max(fun: ReqlModel[T, PK] => ReqlDatum): ReqlModel[T, PK] = {
      Transmuter.transmute[ReqlModel[T, PK]](
        new MaxSelectionOfStreamQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = sel :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MaxOnFiniteStreamOp[T <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T]) {
    def max(): T = {
      Transmuter.transmute[T](
        new MaxFiniteStreamQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = finiteStream :: Nil
          val options = Options.empty
        }
      )
    }

    def max(field: ReqlString): T = {
      Transmuter.transmute[T](
        new MaxFiniteStreamQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = finiteStream :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def max(fun: T => ReqlDatum): T = {
      Transmuter.transmute[T](
        new MaxFiniteStreamQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = finiteStream :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class MaxOnArrayOp[T <: ReqlDatum : Transmuter](val array: ReqlArray[T]) {
    def max()(implicit ev: T <:< ReqlNumber): T = {
      Transmuter.transmute[T](
        new MaxArrayQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = array :: Nil
          val options = Options.empty
        }
      )
    }

    def max(field: ReqlString): T = {
      Transmuter.transmute[T](
        new MaxArrayQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = array :: field :: Nil
          val options = Options.empty
        }
      )
    }

    def max(fun: T => ReqlDatum): T = {
      Transmuter.transmute[T](
        new MaxArrayQuery {
          val command = TermType.MAX
          val string = "max"
          val arguments = array :: Func.wrap1(fun) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  // distinct
  //TODO: if previous query was .between with index this will returns array, without index - stream
  trait DistinctTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait DistinctTableSliceQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait DistinctSelectionOfArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait DistinctSelectionOfStreamQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait DistinctArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  //Stream with and without index
  implicit class DistinctOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    def distinct(indexOptions: IndexOptions = DefaultIndex): DistinctTableQuery[ReqlModel[T, PK]] = new DistinctTableQuery[ReqlModel[T, PK]] {
      val command = TermType.DISTINCT
      val string = "distinct"
      val arguments = table :: Nil
      val options = indexOptions
    }
  }

  //FIXIT - between(index).distinct -> ARRAY; between(no_index).distinct() -> STREAM
  //TODO: it can use same index as orderBy before it and return stream, but how it can be used?
  /*implicit class DistinctOnTableSliceOp(val tableSlice: ReqlTableSlice) extends AnyVal {
    def distinct(): DistinctTableSliceQuery = new DistinctTableSliceQuery {
      val command = TermType.DISTINCT
      val string = "distinct"
      val arguments = tableSlice :: Nil
      val options = Options.empty
    }
  }*/

  implicit class DistinctOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    def distinct(): DistinctSelectionOfArrayQuery[ReqlModel[T, PK]] = new DistinctSelectionOfArrayQuery[ReqlModel[T, PK]] {
      val command = TermType.DISTINCT
      val string = "distinct"
      val arguments = sel :: Nil
      val options = Options.empty
    }
  }

  implicit class DistinctOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    def distinct(): DistinctSelectionOfStreamQuery[ReqlModel[T, PK]] = new DistinctSelectionOfStreamQuery[ReqlModel[T, PK]] {
      val command = TermType.DISTINCT
      val string = "distinct"
      val arguments = sel :: Nil
      val options = Options.empty
    }
  }

  implicit class DistinctOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def distinct(): DistinctArrayQuery[T] = new DistinctArrayQuery[T] {
      val command = TermType.DISTINCT
      val string = "distinct"
      val arguments = array :: Nil
      val options = Options.empty
    }
  }

  // contains
  trait ContainsTableQuery extends ReqlBoolean
  trait ContainsTableSliceQuery extends ReqlBoolean
  trait ContainsSelectionOfArrayQuery extends ReqlBoolean
  trait ContainsSelectionOfStreamQuery extends ReqlBoolean
  trait ContainsArrayQuery extends ReqlBoolean

  implicit class ContainsOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    def contains(predicates: ReqlPredicate[ReqlModel[T, PK]]*): ContainsTableQuery = new ContainsTableQuery {
      val command = TermType.CONTAINS
      val string = "contains"
      val arguments = table :: predicates.toList
      val options = Options.empty
    }
  }

  implicit class ContainsOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    def contains(predicates: ReqlPredicate[ReqlModel[T, PK]]*): ContainsTableSliceQuery = new ContainsTableSliceQuery {
      val command = TermType.CONTAINS
      val string = "contains"
      val arguments = tableSlice :: predicates.toList
      val options = Options.empty
    }
  }

  implicit class ContainsOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    def contains(predicates: ReqlPredicate[ReqlModel[T, PK]]*): ContainsSelectionOfArrayQuery = new ContainsSelectionOfArrayQuery {
      val command = TermType.CONTAINS
      val string = "contains"
      val arguments = sel :: predicates.toList
      val options = Options.empty
    }
  }

  implicit class ContainsOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    def contains(predicates: ReqlPredicate[ReqlModel[T, PK]]*): ContainsSelectionOfStreamQuery = new ContainsSelectionOfStreamQuery {
      val command = TermType.CONTAINS
      val string = "contains"
      val arguments = sel :: predicates.toList
      val options = Options.empty
    }
  }

  implicit class ContainsOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def contains(predicates: ReqlPredicate[T]*): ContainsArrayQuery = new ContainsArrayQuery {
      val command = TermType.CONTAINS
      val string = "contains"
      val arguments = array :: predicates.toList
      val options = Options.empty
    }
  }

}
