package rere.ql.queries

import rere.ql.options.all._
import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.typeclasses.{SequenceUnion, ToPredicate, Transmuter}
import rere.ql.types._

trait TransformationQueries {

  // map
  trait MapTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait MapTableSliceQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait MapSelectionOfArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait MapSelectionOfStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait MapInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait MapFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait MapArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  //TODO: support r.map syntax
  //TODO: support map with different types? stream + array => stream?
  implicit class MapOnTableOp[T0 <: ReqlObject : Transmuter](val table0: ReqlTable[T0]) {
    def map[
      TOut <: ReqlDatum
    ](
      mapFunction: T0 => TOut
    ): MapTableQuery[TOut] = new MapTableQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = table0 :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlObject : Transmuter,
      TOut <: ReqlDatum
    ](
      table1: ReqlTable[T1],
      mapFunction: (T0, T1) => TOut
    ): MapTableQuery[TOut] = new MapTableQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = table0 :: table1 :: Func.wrap2(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlObject : Transmuter,
      T2 <: ReqlObject : Transmuter,
      TOut <: ReqlDatum
    ](
      table1: ReqlTable[T1],
      table2: ReqlTable[T2],
      mapFunction: (T0, T1, T2) => TOut
    ): MapTableQuery[TOut] = new MapTableQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = table0 :: table1 :: table2 :: Func.wrap3(mapFunction) :: Nil
      val options = Options.empty
    }

    //TODO: support bigger arity
  }

  implicit class MapOnTableSliceOp[T0 <: ReqlObject : Transmuter](val tableSlice0: ReqlTableSlice[T0]) {
    def map[
      TOut <: ReqlDatum
    ](
      mapFunction: T0 => TOut
    ): MapTableSliceQuery[TOut] = new MapTableSliceQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = tableSlice0 :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlObject : Transmuter,
      TOut <: ReqlDatum
    ](
      tableSlice1: ReqlTableSlice[T1],
      mapFunction: (T0, T1) => TOut
    ): MapTableSliceQuery[TOut] = new MapTableSliceQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = tableSlice0 :: tableSlice1 :: Func.wrap2(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlObject : Transmuter,
      T2 <: ReqlObject : Transmuter,
      TOut <: ReqlDatum
    ](
      tableSlice1: ReqlTableSlice[T1],
      tableSlice2: ReqlTableSlice[T2],
      mapFunction: (T0, T1, T2) => TOut
    ): MapTableSliceQuery[TOut] = new MapTableSliceQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = tableSlice0 :: tableSlice1 :: tableSlice2 :: Func.wrap3(mapFunction) :: Nil
      val options = Options.empty
    }

    //TODO: support bigger arity
  }

  implicit class MapOnSelectionOfArrayOp[T0 <: ReqlObject : Transmuter](val sel0: ReqlSelectionOfArray[T0]) {
    def map[
      TOut <: ReqlDatum
    ](
      mapFunction: T0 => TOut
    ): MapSelectionOfArrayQuery[TOut] = new MapSelectionOfArrayQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = sel0 :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlObject : Transmuter,
      TOut <: ReqlDatum
    ](
      sel1: ReqlSelectionOfArray[T1],
      mapFunction: (T0, T1) => TOut
    ): MapSelectionOfArrayQuery[TOut] = new MapSelectionOfArrayQuery[TOut] {

      val command = TermType.MAP
      val string = "map"
      val arguments = sel0 :: sel1 :: Func.wrap2(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlObject : Transmuter,
      T2 <: ReqlObject : Transmuter,
      TOut <: ReqlDatum
    ](
      sel1: ReqlSelectionOfArray[T1],
      sel2: ReqlSelectionOfArray[T2],
      mapFunction: (T0, T1, T2) => TOut
    ): MapSelectionOfArrayQuery[TOut] = new MapSelectionOfArrayQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = sel0 :: sel1 :: sel2 :: Func.wrap3(mapFunction) :: Nil
      val options = Options.empty
    }

    //TODO: support bigger arity
  }

  implicit class MapOnSelectionOfStreamOp[T0 <: ReqlObject : Transmuter](val sel0: ReqlSelectionOfStream[T0]) {
    def map[
      TOut <: ReqlDatum
    ](
      mapFunction: T0 => TOut
    ): MapSelectionOfStreamQuery[TOut] = new MapSelectionOfStreamQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = sel0 :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlObject : Transmuter,
      TOut <: ReqlDatum
    ](
      sel1: ReqlSelectionOfStream[T1],
      mapFunction: (T0, T1) => TOut
    ): MapSelectionOfStreamQuery[TOut] = new MapSelectionOfStreamQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = sel0 :: sel1 :: Func.wrap2(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlObject : Transmuter,
      T2 <: ReqlObject : Transmuter,
      TOut <: ReqlDatum
    ](
      sel1: ReqlSelectionOfStream[T1],
      sel2: ReqlSelectionOfStream[T2],
      mapFunction: (T0, T1, T2) => TOut
    ): MapSelectionOfStreamQuery[TOut] = new MapSelectionOfStreamQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = sel0 :: sel1 :: sel2 :: Func.wrap3(mapFunction) :: Nil
      val options = Options.empty
    }

    //TODO: support bigger arity
  }

  //TODO: infinite + finite = finite
  implicit class MapOnInfiniteStreamOp[T0 <: ReqlDatum : Transmuter](val stream0: ReqlInfiniteStream[T0]) {
    def map[
      TOut <: ReqlDatum
    ](
      mapFunction: T0 => TOut
    ): MapInfiniteStreamQuery[TOut] = new MapInfiniteStreamQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = stream0 :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlDatum : Transmuter,
      TOut <: ReqlDatum
    ](
      stream1: ReqlInfiniteStream[T1],
      mapFunction: (T0, T1) => TOut
    ): MapInfiniteStreamQuery[TOut] = new MapInfiniteStreamQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = stream0 :: stream1 :: Func.wrap2(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlDatum : Transmuter,
      T2 <: ReqlDatum : Transmuter,
      TOut <: ReqlDatum
    ](
      stream1: ReqlInfiniteStream[T1],
      stream2: ReqlInfiniteStream[T2],
      mapFunction: (T0, T1, T2) => TOut
    ): MapInfiniteStreamQuery[TOut] = new MapInfiniteStreamQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = stream0 :: stream1 :: stream2 :: Func.wrap3(mapFunction) :: Nil
      val options = Options.empty
    }

    //TODO: support bigger arity
  }

  //TODO: finite + infinite = finite
  //TODO: track all types
  implicit class MapOnFiniteStreamOp[T0 <: ReqlDatum : Transmuter](val stream0: ReqlFiniteStream[T0]) {
    def map[
      TOut <: ReqlDatum
    ](
      mapFunction: T0 => TOut
    ): MapFiniteStreamQuery[TOut] = new MapFiniteStreamQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = stream0 :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlDatum : Transmuter,
      TOut <: ReqlDatum
    ](
      stream1: ReqlFiniteStream[T1],
      mapFunction: (T0, T1) => TOut
    ): MapFiniteStreamQuery[TOut] = new MapFiniteStreamQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = stream0 :: stream1 :: Func.wrap2(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlDatum : Transmuter,
      T2 <: ReqlDatum : Transmuter,
      TOut <: ReqlDatum
    ](
      stream1: ReqlFiniteStream[T1],
      stream2: ReqlFiniteStream[T2],
      mapFunction: (T0, T1, T2) => TOut
    ): MapFiniteStreamQuery[TOut] = new MapFiniteStreamQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = stream0 :: stream1 :: stream2 :: Func.wrap3(mapFunction) :: Nil
      val options = Options.empty
    }

    //TODO: support bigger arity
  }

  implicit class MapOnArrayOp[T0 <: ReqlDatum : Transmuter](val arr0: ReqlArray[T0]) {
    def map[
      TOut <: ReqlDatum
    ](
      mapFunction: T0 => TOut
    ): MapArrayQuery[TOut] = new MapArrayQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = arr0 :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlDatum : Transmuter,
      TOut <: ReqlDatum
    ](
      arr1: ReqlArray[T1],
      mapFunction: (T0, T1) => TOut
    ): MapArrayQuery[TOut] = new MapArrayQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = arr0 :: arr1 :: Func.wrap2(mapFunction) :: Nil
      val options = Options.empty
    }

    def map[
      T1 <: ReqlDatum : Transmuter,
      T2 <: ReqlDatum : Transmuter,
      TOut <: ReqlDatum
    ](
      arr1: ReqlArray[T1],
      arr2: ReqlArray[T2],
      mapFunction: (T0, T1, T2) => TOut
    ): MapArrayQuery[TOut] = new MapArrayQuery[TOut] {
      val command = TermType.MAP
      val string = "map"
      val arguments = arr0 :: arr1 :: arr2 :: Func.wrap3(mapFunction) :: Nil
      val options = Options.empty
    }

    //TODO: support bigger arity
  }

  // with_fields
  trait WithFieldsTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait WithFieldsTableSliceQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait WithFieldsSelectionOfArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait WithFieldsSelectionOfStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait WithFieldsInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait WithFieldsFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait WithFieldsArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class WithFieldsOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def withFields(selectors: ReqlValue*): WithFieldsTableQuery[T] = new WithFieldsTableQuery[T] {
      val command = TermType.WITH_FIELDS
      val string = "with_fields"
      val arguments = table :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithFieldsOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def withFields(selectors: ReqlValue*): WithFieldsTableSliceQuery[T] = new WithFieldsTableSliceQuery[T] {
      val command = TermType.WITH_FIELDS
      val string = "with_fields"
      val arguments = tableSlice :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithFieldsOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def withFields(selectors: ReqlValue*): WithFieldsSelectionOfArrayQuery[T] = new WithFieldsSelectionOfArrayQuery[T] {
      val command = TermType.WITH_FIELDS
      val string = "with_fields"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithFieldsOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def withFields(selectors: ReqlValue*): WithFieldsSelectionOfStreamQuery[T] = new WithFieldsSelectionOfStreamQuery[T] {
      val command = TermType.WITH_FIELDS
      val string = "with_fields"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithFieldsOnInfiniteStreamOp[T <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[T]) {
    def withFields(selectors: ReqlValue*): WithFieldsInfiniteStreamQuery[T] = new WithFieldsInfiniteStreamQuery[T] {
      val command = TermType.WITH_FIELDS
      val string = "with_fields"
      val arguments = infiniteStream :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithFieldsOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def withFields(selectors: ReqlValue*): WithFieldsFiniteStreamQuery[T] = new WithFieldsFiniteStreamQuery[T] {
      val command = TermType.WITH_FIELDS
      val string = "with_fields"
      val arguments = finiteStream :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithFieldsOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def withFields(selectors: ReqlValue*): WithFieldsArrayQuery[T] = new WithFieldsArrayQuery[T] {
      val command = TermType.WITH_FIELDS
      val string = "with_fields"
      val arguments = array :: selectors.toList
      val options = Options.empty
    }
  }

  // concat_map
  trait ConcatMapTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait ConcatMapTableSliceQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait ConcatMapSelectionOfArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait ConcatMapSelectionOfStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait ConcatMapInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait ConcatMapFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait ConcatMapArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  // function return type written not exactly by spec but it's follows the common logic
  implicit class ConcatMapOnTableOp[T <: ReqlObject : Transmuter](val table: ReqlTable[T]) {
    def concatMap[TOut <: ReqlDatum](
      mapFunction: T => ReqlFiniteSequence[TOut]
    ): ConcatMapTableQuery[TOut] = new ConcatMapTableQuery[TOut] {
      val command = TermType.CONCAT_MAP
      val string = "concat_map"
      val arguments = table :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ConcatMapOnTableSliceOp[T <: ReqlObject : Transmuter](val tableSlice: ReqlTableSlice[T]) {
    def concatMap[TOut <: ReqlDatum](
      mapFunction: T => ReqlFiniteSequence[TOut]
    ): ConcatMapTableSliceQuery[TOut] = new ConcatMapTableSliceQuery[TOut] {
      val command = TermType.CONCAT_MAP
      val string = "concat_map"
      val arguments = tableSlice :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ConcatMapOnSelectionOfArrayOp[T <: ReqlObject : Transmuter](val sel: ReqlSelectionOfArray[T]) {
    def concatMap[TOut <: ReqlDatum](
      mapFunction: T => ReqlFiniteSequence[TOut]
    ): ConcatMapSelectionOfArrayQuery[TOut] = new ConcatMapSelectionOfArrayQuery[TOut] {
      val command = TermType.CONCAT_MAP
      val string = "concat_map"
      val arguments = sel :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ConcatMapOnSelectionOfStreamOp[T <: ReqlObject : Transmuter](val sel: ReqlSelectionOfStream[T]) {
    def concatMap[TOut <: ReqlDatum](
      mapFunction: T => ReqlFiniteSequence[TOut]
    ): ConcatMapSelectionOfStreamQuery[TOut] = new ConcatMapSelectionOfStreamQuery[TOut] {
      val command = TermType.CONCAT_MAP
      val string = "concat_map"
      val arguments = sel :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ConcatMapOnInfiniteStreamOp[T <: ReqlDatum : Transmuter](val infiniteStream: ReqlInfiniteStream[T]) {
    def concatMap[TOut <: ReqlDatum](
      mapFunction: T => ReqlFiniteSequence[TOut]
    ): ConcatMapInfiniteStreamQuery[TOut] = new ConcatMapInfiniteStreamQuery[TOut] {
      val command = TermType.CONCAT_MAP
      val string = "concat_map"
      val arguments = infiniteStream :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ConcatMapOnFiniteStreamOp[T <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T]) {
    def concatMap[TOut <: ReqlDatum](
      mapFunction: T => ReqlFiniteSequence[TOut]
    ): ConcatMapFiniteStreamQuery[TOut] = new ConcatMapFiniteStreamQuery[TOut] {
      val command = TermType.CONCAT_MAP
      val string = "concat_map"
      val arguments = finiteStream :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ConcatMapOnArrayOp[T <: ReqlDatum : Transmuter](val array: ReqlArray[T]) {
    def concatMap[TOut <: ReqlDatum](
      mapFunction: T => ReqlFiniteSequence[TOut]
    ): ConcatMapArrayQuery[TOut] = new ConcatMapArrayQuery[TOut] {
      val command = TermType.CONCAT_MAP
      val string = "concat_map"
      val arguments = array :: Func.wrap1(mapFunction) :: Nil
      val options = Options.empty
    }
  }

  // order_by
  trait OrderByTableOnlyWithIndexQuery[T <: ReqlObject] extends ReqlTableSlice[T]
  trait OrderByTableWithOrderingsQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait OrderByTableMixedQuery[T <: ReqlObject] extends ReqlSelectionOfStream[T]
  trait OrderByTableSliceQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait OrderBySelectionOfArrayQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait OrderBySelectionOfStreamQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait OrderByArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  //IMPORTANT: this query return full result, not stream
  implicit class OrderByOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def orderBy(indexOptions: OrderedIndexOptions): OrderByTableOnlyWithIndexQuery[T] = new OrderByTableOnlyWithIndexQuery[T] {
      val command = TermType.ORDER_BY
      val string = "order_by"
      val arguments = table :: Nil
      val options = indexOptions
    }

    def orderBy(orderings: ReqlOrdering*): OrderByTableWithOrderingsQuery[T] = new OrderByTableWithOrderingsQuery[T] {
      val command = TermType.ORDER_BY
      val string = "order_by"
      val arguments = table :: orderings.toList
      val options = Options.empty
    }

    //not by spec but arguments of .between in order in which db will use them
    def orderBy(indexOptions: OrderedIndexOptions,
                orderings: ReqlOrdering*): OrderByTableMixedQuery[T] = new OrderByTableMixedQuery[T] {
      val command = TermType.ORDER_BY
      val string = "order_by"
      val arguments = table :: orderings.toList
      val options = indexOptions
    }
  }

  //TODO: maybe any index operation should no be allowed on table_slice ???
  implicit class OrderByOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    // only orderings, no index because it can work with totally new or already used index. if previous step was already
    // used index "code" ( like between(1, 7, {index: "code"}) ) orderBy({index: "name"}) will fail at runtime.
    // if it needed to used between and orderBy - use orderBy(index).between(x, y). in that case between will use same
    // index as orderBy. in any case 2 different indexes can't be used to query data from one table
    def orderBy(orderings: ReqlOrdering*): OrderByTableSliceQuery[T] = new OrderByTableSliceQuery[T] {
      val command = TermType.ORDER_BY
      val string = "order_by"
      val arguments = tableSlice :: orderings.toList
      val options = Options.empty
    }
  }

  implicit class OrderByOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def orderBy(orderings: ReqlOrdering*): OrderBySelectionOfArrayQuery[T] = new OrderBySelectionOfArrayQuery[T] {
      val command = TermType.ORDER_BY
      val string = "order_by"
      val arguments = sel :: orderings.toList
      val options = Options.empty
    }
  }

  implicit class OrderByOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def orderBy(orderings: ReqlOrdering*): OrderBySelectionOfStreamQuery[T] = new OrderBySelectionOfStreamQuery[T] {
      val command = TermType.ORDER_BY
      val string = "order_by"
      val arguments = sel :: orderings.toList
      val options = Options.empty
    }
  }

  implicit class OrderByOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def orderBy(orderings: ReqlOrdering*): OrderByArrayQuery[T] = new OrderByArrayQuery[T] {
      val command = TermType.ORDER_BY
      val string = "order_by"
      val arguments = array :: orderings.toList
      val options = Options.empty
    }
  }

  // asc
  //TODO: automatically wrap functions to this query?
  implicit class AscOp(val r: ReqlR) {
    def asc(fieldOrIndexName: String): ReqlNameOrdering = new ReqlNameOrdering {
      val command = TermType.ASC
      val string = "asc"
      val arguments = values.expr(fieldOrIndexName) :: Nil
      val options = Options.empty
    }

    def asc(orderingFunction: ReqlDatum => ReqlDatum): ReqlLambdaOrdering = new ReqlLambdaOrdering {
      val command = TermType.ASC
      val string = "asc"
      val arguments = Func.wrap1(orderingFunction) :: Nil
      val options = Options.empty
    }
  }

  // desc
  implicit class DescOp(val r: ReqlR) {
    def desc(fieldOrIndexName: String): ReqlNameOrdering = new ReqlNameOrdering {
      val command = TermType.DESC
      val string = "desc"
      val arguments = values.expr(fieldOrIndexName) :: Nil
      val options = Options.empty
    }

    def desc(orderingFunction: ReqlDatum => ReqlDatum): ReqlLambdaOrdering = new ReqlLambdaOrdering {
      val command = TermType.DESC
      val string = "desc"
      val arguments = Func.wrap1(orderingFunction) :: Nil
      val options = Options.empty
    }
  }

  // skip
  trait SkipTableQuery[T <: ReqlObject] extends ReqlSelectionOfStream[T]
  trait SkipTableSliceQuery[T <: ReqlObject] extends ReqlSelectionOfStream[T]
  trait SkipSelectionOfArrayQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait SkipSelectionOfStreamQuery[T <: ReqlObject] extends ReqlSelectionOfStream[T]
  trait SkipInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait SkipFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait SkipArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class SkipOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def skip(n: ReqlInteger): SkipTableQuery[T] = new SkipTableQuery[T] {
      val command = TermType.SKIP
      val string = "skip"
      val arguments = table :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class SkipOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def skip(n: ReqlInteger): SkipTableSliceQuery[T] = new SkipTableSliceQuery[T] {
      val command = TermType.SKIP
      val string = "skip"
      val arguments = tableSlice :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class SkipOnSelectionOfArrayOp[T <: ReqlObject](val selection: ReqlSelectionOfArray[T]) {
    def skip(n: ReqlInteger): SkipSelectionOfArrayQuery[T] = new SkipSelectionOfArrayQuery[T] {
      val command = TermType.SKIP
      val string = "skip"
      val arguments = selection :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class SkipOnSelectionOfStreamOp[T <: ReqlObject](val selection: ReqlSelectionOfStream[T]) {
    def skip(n: ReqlInteger): SkipSelectionOfStreamQuery[T] = new SkipSelectionOfStreamQuery[T] {
      val command = TermType.SKIP
      val string = "skip"
      val arguments = selection :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class SkipOnInfiniteStreamOp[T <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[T]) {
    def skip(n: ReqlInteger): SkipInfiniteStreamQuery[T] = new SkipInfiniteStreamQuery[T] {
      val command = TermType.SKIP
      val string = "skip"
      val arguments = infiniteStream :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class SkipOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def skip(n: ReqlInteger): SkipFiniteStreamQuery[T] = new SkipFiniteStreamQuery[T] {
      val command = TermType.SKIP
      val string = "skip"
      val arguments = finiteStream :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class SkipOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def skip(n: ReqlInteger): SkipArrayQuery[T] = new SkipArrayQuery[T] {
      val command = TermType.SKIP
      val string = "skip"
      val arguments = array :: n :: Nil
      val options = Options.empty
    }
  }

  // limit
  trait LimitTableQuery[T <: ReqlObject] extends ReqlSelectionOfStream[T]
  trait LimitTableSliceQuery[T <: ReqlObject] extends ReqlSelectionOfStream[T]
  trait LimitSelectionOfArrayQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait LimitSelectionOfStreamQuery[T <: ReqlObject] extends ReqlSelectionOfStream[T]
  trait LimitInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait LimitFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait LimitArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class LimitOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def limit(n: ReqlInteger): LimitTableQuery[T] = new LimitTableQuery[T] {
      val command = TermType.LIMIT
      val string = "limit"
      val arguments = table :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class LimitOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def limit(n: ReqlInteger): LimitTableSliceQuery[T] = new LimitTableSliceQuery[T] {
      val command = TermType.LIMIT
      val string = "limit"
      val arguments = tableSlice :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class LimitOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def limit(n: ReqlInteger): LimitSelectionOfArrayQuery[T] = new LimitSelectionOfArrayQuery[T] {
      val command = TermType.LIMIT
      val string = "limit"
      val arguments = sel :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class LimitOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def limit(n: ReqlInteger): LimitSelectionOfStreamQuery[T] = new LimitSelectionOfStreamQuery[T] {
      val command = TermType.LIMIT
      val string = "limit"
      val arguments = sel :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class LimitOnInfiniteStreamOp[T <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[T]) {
    def limit(n: ReqlInteger): LimitInfiniteStreamQuery[T] = new LimitInfiniteStreamQuery[T] {
      val command = TermType.LIMIT
      val string = "limit"
      val arguments = infiniteStream :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class LimitOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def limit(n: ReqlInteger): LimitFiniteStreamQuery[T] = new LimitFiniteStreamQuery[T] {
      val command = TermType.LIMIT
      val string = "limit"
      val arguments = finiteStream :: n :: Nil
      val options = Options.empty
    }
  }

  implicit class LimitOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def limit(n: ReqlInteger): LimitArrayQuery[T] = new LimitArrayQuery[T] {
      val command = TermType.LIMIT
      val string = "limit"
      val arguments = array :: n :: Nil
      val options = Options.empty
    }
  }

  //slice
  trait SliceArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait SliceBinaryQuery extends ReqlBinary
  trait SliceStringQuery extends ReqlString
  //TODO: make impl for other types
  //r.table("tv_shows").slice(1).typeOf() -> "SELECTION<STREAM>"

  implicit class SliceOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def slice(startOffset: ReqlInteger): SliceArrayQuery[T] = new SliceArrayQuery[T] {
      val command = TermType.SLICE
      val string = "slice"
      val arguments = array :: startOffset :: Nil
      val options = Options.empty
    }

    def slice(startOffset: ReqlInteger,
              boundsOptions: BoundsOptions): SliceArrayQuery[T] = new SliceArrayQuery[T] {
      val command = TermType.SLICE
      val string = "slice"
      val arguments = array :: startOffset :: Nil
      val options = boundsOptions
    }

    def slice(startOffset: ReqlInteger,
              endOffset: ReqlInteger): SliceArrayQuery[T] = new SliceArrayQuery[T] {
      val command = TermType.SLICE
      val string = "slice"
      val arguments = array :: startOffset :: endOffset :: Nil
      val options = Options.empty
    }

    def slice(startOffset: ReqlInteger,
              endOffset: ReqlInteger,
              boundsOptions: BoundsOptions): SliceArrayQuery[T] = new SliceArrayQuery[T] {
      val command = TermType.SLICE
      val string = "slice"
      val arguments = array :: startOffset :: endOffset :: Nil
      val options = boundsOptions
    }
  }

  // nth
  trait NthTableQuery[T <: ReqlObject] extends ReqlSelectionOfObject[T]
  trait NthTableSliceQuery[T <: ReqlObject] extends ReqlSelectionOfObject[T]
  trait NthSelectionOfArrayQuery[T <: ReqlObject] extends ReqlSelectionOfObject[T]
  trait NthSelectionOfStreamQuery[T <: ReqlObject] extends ReqlSelectionOfObject[T]
  trait NthArrayQuery extends ReqlDatum

  implicit class NthOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def nth(index: ReqlInteger): NthTableQuery[T] = new NthTableQuery[T] {
      val command = TermType.NTH
      val string = "nth"
      val arguments = table :: index :: Nil
      val options = Options.empty
    }
  }

  implicit class NthOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def nth(index: ReqlInteger): NthTableSliceQuery[T] = new NthTableSliceQuery[T] {
      val command = TermType.NTH
      val string = "nth"
      val arguments = tableSlice :: index :: Nil
      val options = Options.empty
    }
  }

  implicit class NthOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def nth(index: ReqlInteger): NthSelectionOfArrayQuery[T] = new NthSelectionOfArrayQuery[T] {
      val command = TermType.NTH
      val string = "nth"
      val arguments = sel :: index :: Nil
      val options = Options.empty
    }
  }

  implicit class NthOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def nth(index: ReqlInteger): NthSelectionOfStreamQuery[T] = new NthSelectionOfStreamQuery[T] {
      val command = TermType.NTH
      val string = "nth"
      val arguments = sel :: index :: Nil
      val options = Options.empty
    }
  }

  //TODO: return T instead NthArrayQuery
  implicit class NthOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def nth(index: ReqlInteger): NthArrayQuery = new NthArrayQuery {
      val command = TermType.NTH
      val string = "nth"
      val arguments = array :: index :: Nil
      val options = Options.empty
    }
  }

  // offsets_of
  trait OffsetsOfTableQuery extends ReqlFiniteStream[ReqlInteger]
  trait OffsetsOfTableSliceQuery extends ReqlFiniteStream[ReqlInteger]
  trait OffsetsOfSelectionOfArrayQuery extends ReqlArray[ReqlInteger]
  trait OffsetsOfSelectionOfStreamQuery extends ReqlFiniteStream[ReqlInteger]
  trait OffsetsOfInfiniteStreamQuery extends ReqlInfiniteStream[ReqlInteger]
  trait OffsetsOfFiniteStreamQuery extends ReqlFiniteStream[ReqlInteger]
  trait OffsetsOfArrayQuery extends ReqlArray[ReqlInteger]

  implicit class OffsetsOfTableOp[T <: ReqlObject : Transmuter](val table: ReqlTable[T]) {
    def offsetsOf(datum: T): OffsetsOfTableQuery = new OffsetsOfTableQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = table :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def offsetsOf(function: T => ReqlBoolean): OffsetsOfTableQuery = new OffsetsOfTableQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = table :: ToPredicate(function) :: Nil
      val options = Options.empty
    }
  }

  implicit class OffsetsOfTableSliceOp[T <: ReqlObject : Transmuter](val tableSlice: ReqlTableSlice[T]) {
    def offsetsOf(datum: T): OffsetsOfTableSliceQuery = new OffsetsOfTableSliceQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = tableSlice :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def offsetsOf(function: T => ReqlBoolean): OffsetsOfTableSliceQuery = new OffsetsOfTableSliceQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = tableSlice :: ToPredicate(function) :: Nil
      val options = Options.empty
    }
  }

  implicit class OffsetsOfSelectionOfArrayOp[T <: ReqlObject : Transmuter](val sel: ReqlSelectionOfArray[T]) {
    def offsetsOf(datum: T): OffsetsOfSelectionOfArrayQuery = new OffsetsOfSelectionOfArrayQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = sel :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def offsetsOf(function: T => ReqlBoolean): OffsetsOfSelectionOfArrayQuery = new OffsetsOfSelectionOfArrayQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = sel :: ToPredicate(function) :: Nil
      val options = Options.empty
    }
  }

  implicit class OffsetsOfSelectionOfStreamOp[T <: ReqlObject : Transmuter](val sel: ReqlSelectionOfStream[T]) {
    def offsetsOf(datum: T): OffsetsOfSelectionOfStreamQuery = new OffsetsOfSelectionOfStreamQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = sel :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def offsetsOf(function: T => ReqlBoolean): OffsetsOfSelectionOfStreamQuery = new OffsetsOfSelectionOfStreamQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = sel :: ToPredicate(function) :: Nil
      val options = Options.empty
    }
  }

  implicit class OffsetsOfInfiniteStreamOp[T <: ReqlDatum : Transmuter](val infiniteStream: ReqlInfiniteStream[T]) {
    def offsetsOf(datum: T): OffsetsOfInfiniteStreamQuery = new OffsetsOfInfiniteStreamQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = infiniteStream :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def offsetsOf(function: T => ReqlBoolean): OffsetsOfInfiniteStreamQuery = new OffsetsOfInfiniteStreamQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = infiniteStream :: ToPredicate(function) :: Nil
      val options = Options.empty
    }
  }

  implicit class OffsetsOfFiniteStreamOp[T <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T]) {
    def offsetsOf(datum: T): OffsetsOfFiniteStreamQuery = new OffsetsOfFiniteStreamQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = finiteStream :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def offsetsOf(function: T => ReqlBoolean): OffsetsOfFiniteStreamQuery = new OffsetsOfFiniteStreamQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = finiteStream :: ToPredicate(function) :: Nil
      val options = Options.empty
    }
  }

  implicit class OffsetsOfArrayOp[T <: ReqlDatum : Transmuter](val array: ReqlArray[T]) {
    def offsetsOf(datum: T): OffsetsOfArrayQuery = new OffsetsOfArrayQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = array :: ToPredicate(datum) :: Nil
      val options = Options.empty
    }

    def offsetsOf(function: T => ReqlBoolean): OffsetsOfArrayQuery = new OffsetsOfArrayQuery {
      val command = TermType.OFFSETS_OF
      val string = "offsets_of"
      val arguments = array :: ToPredicate(function) :: Nil
      val options = Options.empty
    }
  }

  // is_empty
  trait IsEmptyTableQuery extends ReqlBoolean
  trait IsEmptyTableSliceQuery extends ReqlBoolean
  trait IsEmptySelectionOfArrayQuery extends ReqlBoolean
  trait IsEmptySelectionOfStreamQuery extends ReqlBoolean
  trait IsEmptyInfiniteStreamQuery extends ReqlBoolean
  trait IsEmptyFiniteStreamQuery extends ReqlBoolean
  trait IsEmptyArrayQuery extends ReqlBoolean

  implicit class IsEmptyOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def isEmpty(): IsEmptyTableQuery = new IsEmptyTableQuery {
      val command = TermType.IS_EMPTY
      val string = "is_empty"
      val arguments = table :: Nil
      val options = Options.empty
    }
  }

  implicit class IsEmptyOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def isEmpty(): IsEmptyTableSliceQuery = new IsEmptyTableSliceQuery {
      val command = TermType.IS_EMPTY
      val string = "is_empty"
      val arguments = tableSlice :: Nil
      val options = Options.empty
    }
  }

  implicit class IsEmptyOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def isEmpty(): IsEmptySelectionOfArrayQuery = new IsEmptySelectionOfArrayQuery {
      val command = TermType.IS_EMPTY
      val string = "is_empty"
      val arguments = sel :: Nil
      val options = Options.empty
    }
  }

  implicit class IsEmptyOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def isEmpty(): IsEmptySelectionOfStreamQuery = new IsEmptySelectionOfStreamQuery {
      val command = TermType.IS_EMPTY
      val string = "is_empty"
      val arguments = sel :: Nil
      val options = Options.empty
    }
  }

  //TODO: db can execute isEmpty on infinite stream. by default dataexplorer wait 500ms, js driver - forever
  implicit class IsEmptyInfiniteStreamOp[T <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[T]) {
    def isEmpty(): IsEmptyInfiniteStreamQuery = new IsEmptyInfiniteStreamQuery {
      val command = TermType.IS_EMPTY
      val string = "is_empty"
      val arguments = infiniteStream :: Nil
      val options = Options.empty
    }
  }

  implicit class IsEmptyFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def isEmpty(): IsEmptyFiniteStreamQuery = new IsEmptyFiniteStreamQuery {
      val command = TermType.IS_EMPTY
      val string = "is_empty"
      val arguments = finiteStream :: Nil
      val options = Options.empty
    }
  }

  implicit class IsEmptyArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def isEmpty(): IsEmptyArrayQuery = new IsEmptyArrayQuery {
      val command = TermType.IS_EMPTY
      val string = "is_empty"
      val arguments = array :: Nil
      val options = Options.empty
    }
  }

  // union
  //TODO: maybe allow use interleave option only on ordered sequences?
  trait UnionQuery extends ReqlExpr {
    val command = TermType.UNION
    val string = "union"
  }

  implicit class UnionOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Seq[Other],
      interleaveOptions: InterleaveOptions = Mix)(
      implicit
      u: SequenceUnion.Aux[T, ReqlTable[T], Other, UnionOut],
      t: Transmuter[UnionOut]
    ): UnionOut = {
      t.transmute(
        new UnionQuery {
          val arguments = table :: otherSeqs.toList
          val options = interleaveOptions
        }
      )
    }
  }

  implicit class UnionOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Seq[Other],
      interleaveOptions: InterleaveOptions = Mix)(
      implicit
      u: SequenceUnion.Aux[T, ReqlTableSlice[T], Other, UnionOut],
      t: Transmuter[UnionOut]
    ): UnionOut = {
      t.transmute(
        new UnionQuery {
          val arguments = tableSlice :: otherSeqs.toList
          val options = interleaveOptions
        }
      )
    }
  }

  implicit class UnionOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Seq[Other],
      interleaveOptions: InterleaveOptions = Mix)(
      implicit
      u: SequenceUnion.Aux[T, ReqlSelectionOfStream[T], Other, UnionOut],
      t: Transmuter[UnionOut]
    ): UnionOut = {
      t.transmute(
        new UnionQuery {
          val arguments = sel :: otherSeqs.toList
          val options = interleaveOptions
        }
      )
    }
  }

  implicit class UnionOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Seq[Other],
      interleaveOptions: InterleaveOptions = Mix)(
      implicit
      u: SequenceUnion.Aux[T, ReqlSelectionOfArray[T], Other, UnionOut],
      t: Transmuter[UnionOut]
    ): UnionOut = {
      t.transmute(
        new UnionQuery {
          val arguments = sel :: otherSeqs.toList
          val options = interleaveOptions
        }
      )
    }
  }

  implicit class UnionOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Seq[Other],
      interleaveOptions: InterleaveOptions = Mix)(
      implicit
      u: SequenceUnion.Aux[T, ReqlFiniteStream[T], Other, UnionOut],
      t: Transmuter[UnionOut]
    ): UnionOut = {
      t.transmute(
        new UnionQuery {
          val arguments = finiteStream :: otherSeqs.toList
          val options = interleaveOptions
        }
      )
    }
  }

  implicit class UnionOnInfiniteStreamOp[T <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Seq[Other],
      interleaveOptions: InterleaveOptions = Mix)(
      implicit
      u: SequenceUnion.Aux[T, ReqlInfiniteStream[T], Other, UnionOut],
      t: Transmuter[UnionOut]
    ): UnionOut = {
      t.transmute(
        new UnionQuery {
          val arguments = infiniteStream :: otherSeqs.toList
          val options = interleaveOptions
        }
      )
    }
  }

  implicit class UnionOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Seq[Other],
      interleaveOptions: InterleaveOptions = Mix)(
      implicit
      u: SequenceUnion.Aux[T, ReqlArray[T], Other, UnionOut],
      t: Transmuter[UnionOut]
    ): UnionOut = {
      t.transmute(
        new UnionQuery {
          val arguments = array :: otherSeqs.toList
          val options = interleaveOptions
        }
      )
    }
  }

  // sample
  trait SampleTableQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait SampleTableSliceQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait SampleSelectionOfArrayQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait SampleSelectionOfStreamQuery[T <: ReqlObject] extends ReqlSelectionOfArray[T]
  trait SampleFiniteStreamQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait SampleArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class SampleOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def sample(size: ReqlInteger): SampleTableQuery[T] = new SampleTableQuery[T] {
      val command = TermType.SAMPLE
      val string = "sample"
      val arguments = table :: size :: Nil
      val options = Options.empty
    }
  }

  implicit class SampleOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def sample(size: ReqlInteger): SampleTableSliceQuery[T] = new SampleTableSliceQuery[T] {
      val command = TermType.SAMPLE
      val string = "sample"
      val arguments = tableSlice :: size :: Nil
      val options = Options.empty
    }
  }

  implicit class SampleOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def sample(size: ReqlInteger): SampleSelectionOfArrayQuery[T] = new SampleSelectionOfArrayQuery[T] {
      val command = TermType.SAMPLE
      val string = "sample"
      val arguments = sel :: size :: Nil
      val options = Options.empty
    }
  }

  implicit class SampleOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def sample(size: ReqlInteger): SampleSelectionOfStreamQuery[T] = new SampleSelectionOfStreamQuery[T] {
      val command = TermType.SAMPLE
      val string = "sample"
      val arguments = sel :: size :: Nil
      val options = Options.empty
    }
  }

  implicit class SampleOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def sample(size: ReqlInteger): SampleFiniteStreamQuery[T] = new SampleFiniteStreamQuery[T] {
      val command = TermType.SAMPLE
      val string = "sample"
      val arguments = finiteStream :: size :: Nil
      val options = Options.empty
    }
  }

  implicit class SampleOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def sample(size: ReqlInteger): SampleArrayQuery[T] = new SampleArrayQuery[T] {
      val command = TermType.SAMPLE
      val string = "sample"
      val arguments = array :: size :: Nil
      val options = Options.empty
    }
  }

}
