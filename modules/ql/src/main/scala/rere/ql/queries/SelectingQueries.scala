package rere.ql.queries

import rere.ql.options.all._
import rere.ql.options.{ComposableOptions, Options}
import rere.ql.ql2.Term.TermType
import rere.ql.shapes.{ModelShape, ReqlModel}
import rere.ql.typeclasses.{ToFilterPredicate, Transmuter}
import rere.ql.types._

trait SelectingQueries {

  // get
  trait GetQuery[T, PK] extends ReqlSelectionOfObject[T, PK]

  implicit class GetOp[T, PK](val table: ReqlTable[T, PK])(
    implicit shape: ModelShape[T, PK]
  ) {
    def get(key: PK): GetQuery[T, PK] = new GetQuery[T, PK] {
      val command = TermType.GET
      val string = "get"
      val arguments = table :: shape.toReqlPrimaryKey(key) :: Nil
      val options = Options.empty
    }
  }

  // get_all
  trait GetAllQuery[T, PK] extends ReqlSelectionOfStream[T, PK]
  //TODO:  An orderBy command that uses a secondary index cannot be chained after getAll. You can only chain it after a table command. However, you can chain orderBy after a between command provided it uses the same index

  implicit class GetAllOp[T, PK](val table: ReqlTable[T, PK])(
    implicit shape: ModelShape[T, PK]
  ) {
    //TODO: make index option type safe (RqlIndexExpr???, r.index???)
    def getAll(keys: PK*): GetAllQuery[T, PK] = new GetAllQuery[T, PK] {
      val command = TermType.GET_ALL
      val string = "get_all"
      val arguments = table :: keys.map(shape.toReqlPrimaryKey).toList
      val options = Options.empty
    }

    def getAll(indexOptions: IndexOptions,
               keys: ReqlValue*): GetAllQuery[T, PK] = new GetAllQuery[T, PK] {
      val command = TermType.GET_ALL
      val string = "get_all"
      val arguments = table :: keys.toList
      val options = indexOptions
    }

    def getAll(args: ReqlArgs): GetAllQuery[T, PK] = new GetAllQuery[T, PK] {
      val command = TermType.GET_ALL
      val string = "get_all"
      val arguments = table :: args :: Nil
      val options = Options.empty
    }

    def getAll(indexOptions: IndexOptions,
               args: ReqlArgs): GetAllQuery[T, PK] = new GetAllQuery[T, PK] {
      val command = TermType.GET_ALL
      val string = "get_all"
      val arguments = table :: args :: Nil
      val options = indexOptions
    }
  }

  // between
  trait BetweenTableQuery[T, PK] extends ReqlTableSlice[T, PK]
  trait BetweenTableSliceQuery[T, PK] extends ReqlTableSlice[T, PK]

  implicit class BetweenOnTableOp[T, PK](val table: ReqlTable[T, PK]) {
    def between(lowerKey: ReqlValue,
                upperKey: ReqlValue,
                boundsOptions: BoundsOptions = DefaultBounds,
                indexOptions: IndexOptions = DefaultIndex): BetweenTableQuery[T, PK] = new BetweenTableQuery[T, PK] {
      val command = TermType.BETWEEN
      val string = "between"
      val arguments = table :: lowerKey :: upperKey :: Nil
      val options = ComposableOptions.compose(boundsOptions, indexOptions)
    }
  }

  implicit class BetweenOnTableSliceOp[T, PK](val tableSlice: ReqlTableSlice[T, PK]) {
    //use index on tableSlice is not safe - if index differs from what has been used in previous step whole query will fail
    def between(lowerKey: ReqlValue,
                upperKey: ReqlValue,
                boundsOptions: BoundsOptions = DefaultBounds): BetweenTableSliceQuery[T, PK] = new BetweenTableSliceQuery[T, PK] {
      val command = TermType.BETWEEN
      val string = "between"
      val arguments = tableSlice :: lowerKey :: upperKey :: Nil
      val options = boundsOptions
    }
  }

  // minval
  trait MinValQuery extends ReqlValue

  //TODO: create constants only once?
  //TODO: maybe move all contants in one implicit class???
  implicit class MinValOp(val r: ReqlR) {
    def minval: MinValQuery = new MinValQuery {
      val command = TermType.MINVAL
      val string = "minval"
      val arguments = Nil
      val options = Options.empty
    }
  }

  // maxval
  trait MaxValQuery extends ReqlValue

  implicit class MaxValOp(val r: ReqlR) {
    def maxval: MaxValQuery = new MaxValQuery {
      val command = TermType.MAXVAL
      val string = "maxval"
      val arguments = Nil
      val options = Options.empty
    }
  }

  // filter
  trait FilterTableQuery[T, PK] extends ReqlSelectionOfStream[T, PK]
  trait FilterTableSliceQuery[T, PK] extends ReqlSelectionOfStream[T, PK]
  trait FilterSelectionOfArrayQuery[T, PK] extends ReqlSelectionOfArray[T, PK]
  trait FilterSelectionOfStreamQuery[T, PK] extends ReqlSelectionOfStream[T, PK]
  trait FilterInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait FilterFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait FilterArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class FilterOnTableOp[T, PK](val table: ReqlTable[T, PK])(
    implicit shape: ModelShape[T, PK]
  ) {
    def filter(obj: ReqlModel[T, PK]): FilterTableQuery[T, PK] = new FilterTableQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = table :: ToFilterPredicate(obj) :: Nil
      val options = Options.empty
    }

    def filter(obj: ReqlModel[T, PK],
               skip: Skip.type): FilterTableQuery[T, PK] = new FilterTableQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = table :: ToFilterPredicate(obj) :: Nil
      val options = skip
    }

    def filter(obj: ReqlModel[T, PK],
               noSkip: NoSkip.type): FilterTableQuery[T, PK] = new FilterTableQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = table :: ToFilterPredicate(obj) :: Nil
      val options = noSkip
    }

    def filter(obj: ReqlModel[T, PK],
               rethrowError: RethrowError.type): FilterTableQuery[T, PK] = new FilterTableQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = table :: ToFilterPredicate(obj) :: Nil
      val options = rethrowError
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean): FilterTableQuery[T, PK] = new FilterTableQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = table :: ToFilterPredicate(function) :: Nil
      val options = Options.empty
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               skip: Skip.type): FilterTableQuery[T, PK] = new FilterTableQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = table :: ToFilterPredicate(function) :: Nil
      val options = skip
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               noSkip: NoSkip.type): FilterTableQuery[T, PK] = new FilterTableQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = table :: ToFilterPredicate(function) :: Nil
      val options = noSkip
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               rethrowError: RethrowError.type): FilterTableQuery[T, PK] = new FilterTableQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = table :: ToFilterPredicate(function) :: Nil
      val options = rethrowError
    }
  }

  implicit class FilterOnTableSliceOp[T, PK](val tableSlice: ReqlTableSlice[T, PK])(
    implicit shape: ModelShape[T, PK]
  ) {
    def filter(obj: ReqlModel[T, PK]): FilterTableSliceQuery[T, PK] = new FilterTableSliceQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = tableSlice :: ToFilterPredicate(obj) :: Nil
      val options = Options.empty
    }

    def filter(obj: ReqlModel[T, PK],
               skip: Skip.type): FilterTableSliceQuery[T, PK] = new FilterTableSliceQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = tableSlice :: ToFilterPredicate(obj) :: Nil
      val options = skip
    }

    def filter(obj: ReqlModel[T, PK],
               noSkip: NoSkip.type): FilterTableSliceQuery[T, PK] = new FilterTableSliceQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = tableSlice :: ToFilterPredicate(obj) :: Nil
      val options = noSkip
    }

    def filter(obj: ReqlModel[T, PK],
               rethrowError: RethrowError.type): FilterTableSliceQuery[T, PK] = new FilterTableSliceQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = tableSlice :: ToFilterPredicate(obj) :: Nil
      val options = rethrowError
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean): FilterTableSliceQuery[T, PK] = new FilterTableSliceQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = tableSlice :: ToFilterPredicate(function) :: Nil
      val options = Options.empty
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               skip: Skip.type): FilterTableSliceQuery[T, PK] = new FilterTableSliceQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = tableSlice :: ToFilterPredicate(function) :: Nil
      val options = skip
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               noSkip: NoSkip.type): FilterTableSliceQuery[T, PK] = new FilterTableSliceQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = tableSlice :: ToFilterPredicate(function) :: Nil
      val options = noSkip
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               rethrowError: RethrowError.type): FilterTableSliceQuery[T, PK] = new FilterTableSliceQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = tableSlice :: ToFilterPredicate(function) :: Nil
      val options = rethrowError
    }
  }

  implicit class FilterOnSelectionOfArrayOp[T, PK](val sel: ReqlSelectionOfArray[T, PK])(
    implicit shape: ModelShape[T, PK]
  ) {
    def filter(obj: ReqlModel[T, PK]): FilterSelectionOfArrayQuery[T, PK] = new FilterSelectionOfArrayQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(obj) :: Nil
      val options = Options.empty
    }

    def filter(obj: ReqlModel[T, PK],
               skip: Skip.type): FilterSelectionOfArrayQuery[T, PK] = new FilterSelectionOfArrayQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(obj) :: Nil
      val options = skip
    }

    def filter(obj: ReqlModel[T, PK],
               noSkip: NoSkip.type): FilterSelectionOfArrayQuery[T, PK] = new FilterSelectionOfArrayQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(obj) :: Nil
      val options = noSkip
    }

    def filter(obj: ReqlModel[T, PK],
               rethrowError: RethrowError.type): FilterSelectionOfArrayQuery[T, PK] = new FilterSelectionOfArrayQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(obj) :: Nil
      val options = rethrowError
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean): FilterSelectionOfArrayQuery[T, PK] = new FilterSelectionOfArrayQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(function) :: Nil
      val options = Options.empty
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               skip: Skip.type): FilterSelectionOfArrayQuery[T, PK] = new FilterSelectionOfArrayQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(function) :: Nil
      val options = skip
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               noSkip: NoSkip.type): FilterSelectionOfArrayQuery[T, PK] = new FilterSelectionOfArrayQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(function) :: Nil
      val options = noSkip
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               rethrowError: RethrowError.type): FilterSelectionOfArrayQuery[T, PK] = new FilterSelectionOfArrayQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(function) :: Nil
      val options = rethrowError
    }
  }

  implicit class FilterOnSelectionOfStreamOp[T, PK](val sel: ReqlSelectionOfStream[T, PK])(
    implicit shape: ModelShape[T, PK]
  ) {
    def filter(obj: ReqlModel[T, PK]): FilterSelectionOfStreamQuery[T, PK] = new FilterSelectionOfStreamQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(obj) :: Nil
      val options = Options.empty
    }

    def filter(obj: ReqlModel[T, PK],
               skip: Skip.type): FilterSelectionOfStreamQuery[T, PK] = new FilterSelectionOfStreamQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(obj) :: Nil
      val options = skip
    }

    def filter(obj: ReqlModel[T, PK],
               noSkip: NoSkip.type): FilterSelectionOfStreamQuery[T, PK] = new FilterSelectionOfStreamQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(obj) :: Nil
      val options = noSkip
    }

    def filter(obj: ReqlModel[T, PK],
               rethrowError: RethrowError.type): FilterSelectionOfStreamQuery[T, PK] = new FilterSelectionOfStreamQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(obj) :: Nil
      val options = rethrowError
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean): FilterSelectionOfStreamQuery[T, PK] = new FilterSelectionOfStreamQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(function) :: Nil
      val options = Options.empty
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               skip: Skip.type): FilterSelectionOfStreamQuery[T, PK] = new FilterSelectionOfStreamQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(function) :: Nil
      val options = skip
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               noSkip: NoSkip.type): FilterSelectionOfStreamQuery[T, PK] = new FilterSelectionOfStreamQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(function) :: Nil
      val options = noSkip
    }

    def filter(function: ReqlModel[T, PK] => ReqlBoolean,
               rethrowError: RethrowError.type): FilterSelectionOfStreamQuery[T, PK] = new FilterSelectionOfStreamQuery[T, PK] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = sel :: ToFilterPredicate(function) :: Nil
      val options = rethrowError
    }
  }

  implicit class FilterObjectOnInfiniteStreamOp[T <: ReqlObject](val infiniteStream: ReqlInfiniteStream[T]) {
    def filter(obj: T): FilterInfiniteStreamQuery[T] = new FilterInfiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = infiniteStream :: ToFilterPredicate(obj) :: Nil
      val options = Options.empty
    }

    def filter(obj: T,
               skip: Skip.type): FilterInfiniteStreamQuery[T] = new FilterInfiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = infiniteStream :: ToFilterPredicate(obj) :: Nil
      val options = skip
    }

    def filter(obj: T,
               noSkip: NoSkip.type): FilterInfiniteStreamQuery[T] = new FilterInfiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = infiniteStream :: ToFilterPredicate(obj) :: Nil
      val options = noSkip
    }

    def filter(obj: T,
               rethrowError: RethrowError.type): FilterInfiniteStreamQuery[T] = new FilterInfiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = infiniteStream :: ToFilterPredicate(obj) :: Nil
      val options = rethrowError
    }
  }

  implicit class FilterDatumOnInfiniteStreamOp[T <: ReqlDatum : Transmuter](val infiniteStream: ReqlInfiniteStream[T]) {
    def filter(function: T => ReqlBoolean): FilterInfiniteStreamQuery[T] = new FilterInfiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = infiniteStream :: ToFilterPredicate(function) :: Nil
      val options = Options.empty
    }

    def filter(function: T => ReqlBoolean,
               skip: Skip.type): FilterInfiniteStreamQuery[T] = new FilterInfiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = infiniteStream :: ToFilterPredicate(function) :: Nil
      val options = skip
    }

    def filter(function: T => ReqlBoolean,
               noSkip: NoSkip.type): FilterInfiniteStreamQuery[T] = new FilterInfiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = infiniteStream :: ToFilterPredicate(function) :: Nil
      val options = noSkip
    }

    def filter(function: T => ReqlBoolean,
               rethrowError: RethrowError.type): FilterInfiniteStreamQuery[T] = new FilterInfiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = infiniteStream :: ToFilterPredicate(function) :: Nil
      val options = rethrowError
    }
  }

  implicit class FilterObjectOnFiniteStreamOp[T <: ReqlObject](val finiteStream: ReqlFiniteStream[T]) {
    def filter(obj: T): FilterFiniteStreamQuery[T] = new FilterFiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = finiteStream :: ToFilterPredicate(obj) :: Nil
      val options = Options.empty
    }

    def filter(obj: T,
               skip: Skip.type): FilterFiniteStreamQuery[T] = new FilterFiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = finiteStream :: ToFilterPredicate(obj) :: Nil
      val options = skip
    }

    def filter(obj: T,
               noSkip: NoSkip.type): FilterFiniteStreamQuery[T] = new FilterFiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = finiteStream :: ToFilterPredicate(obj) :: Nil
      val options = noSkip
    }

    def filter(obj: T,
               rethrowError: RethrowError.type): FilterFiniteStreamQuery[T] = new FilterFiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = finiteStream :: ToFilterPredicate(obj) :: Nil
      val options = rethrowError
    }
  }

  implicit class FilterDatumOnFiniteStreamOp[T <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T]) {
    def filter(function: T => ReqlBoolean): FilterFiniteStreamQuery[T] = new FilterFiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = finiteStream :: ToFilterPredicate(function) :: Nil
      val options = Options.empty
    }

    def filter(function: T => ReqlBoolean,
               skip: Skip.type): FilterFiniteStreamQuery[T] = new FilterFiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = finiteStream :: ToFilterPredicate(function) :: Nil
      val options = skip
    }

    def filter(function: T => ReqlBoolean,
               noSkip: NoSkip.type): FilterFiniteStreamQuery[T] = new FilterFiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = finiteStream :: ToFilterPredicate(function) :: Nil
      val options = noSkip
    }

    def filter(function: T => ReqlBoolean,
               rethrowError: RethrowError.type): FilterFiniteStreamQuery[T] = new FilterFiniteStreamQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = finiteStream :: ToFilterPredicate(function) :: Nil
      val options = rethrowError
    }
  }

  implicit class FilterObjectOnArrayOp[T <: ReqlObject](val array: ReqlArray[T]) {
    def filter(obj: T): FilterArrayQuery[T] = new FilterArrayQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = array :: ToFilterPredicate(obj) :: Nil
      val options = Options.empty
    }

    def filter(obj: T,
               skip: Skip.type): FilterArrayQuery[T] = new FilterArrayQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = array :: ToFilterPredicate(obj) :: Nil
      val options = skip
    }

    def filter(obj: T,
               noSkip: NoSkip.type): FilterArrayQuery[T] = new FilterArrayQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = array :: ToFilterPredicate(obj) :: Nil
      val options = noSkip
    }

    def filter(obj: T,
               rethrowError: RethrowError.type): FilterArrayQuery[T] = new FilterArrayQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = array :: ToFilterPredicate(obj) :: Nil
      val options = rethrowError
    }
  }

  implicit class FilterDatumOnArrayOp[T <: ReqlDatum : Transmuter](val array: ReqlArray[T]) {
    def filter(function: T => ReqlBoolean): FilterArrayQuery[T] = new FilterArrayQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = array :: ToFilterPredicate(function) :: Nil
      val options = Options.empty
    }

    def filter(function: T => ReqlBoolean,
               skip: Skip.type): FilterArrayQuery[T] = new FilterArrayQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = array :: ToFilterPredicate(function) :: Nil
      val options = skip
    }

    def filter(function: T => ReqlBoolean,
               noSkip: NoSkip.type): FilterArrayQuery[T] = new FilterArrayQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = array :: ToFilterPredicate(function) :: Nil
      val options = noSkip
    }

    def filter(function: T => ReqlBoolean,
               rethrowError: RethrowError.type): FilterArrayQuery[T] = new FilterArrayQuery[T] {
      val command = TermType.FILTER
      val string = "filter"
      val arguments = array :: ToFilterPredicate(function) :: Nil
      val options = rethrowError
    }
  }

}
