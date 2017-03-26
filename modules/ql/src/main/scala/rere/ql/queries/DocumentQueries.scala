package rere.ql.queries

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait DocumentQueries {

  // row
  //TODO: can't prove at compile time what query contains r.row and transform it to lambda (need effect system or something similar)
  //TODO: IMPLICIT_VAR implement after lambda
  /*implicit class ImplicitVarOnROp(val r: ReqlR) extends AnyVal {
    def row: ImplicitVarQuery = new ImplicitVarQuery {
      val command = TermType.IMPLICIT_VAR
      val string = "row"
      val arguments = Nil
      val options = Options.empty
    }

    //call brackets directly on row
    def row(attr: ReqlString): BracketRowQuery = new BracketRowQuery {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = row :: attr :: Nil
      val options = Options.empty
    }

    def row(index: ReqlNumber): BracketRowQuery = new BracketRowQuery {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = row :: index :: Nil
      val options = Options.empty
    }
  }*/

  /*implicit class ImplicitVarOnItSelfOp(val implVar: ImplicitVarQuery) extends AnyVal {
    def apply(rowName: ReqlString): ImplicitVarQuery = new ImplicitVarQuery {
      val command = TermType.IMPLICIT_VAR
      val string = "row"
      val arguments = implVar.arguments :+ rowName
      val options = Options.empty
    }
  }*/

  // pluck
  trait PluckTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait PluckTableSliceQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait PluckSelectionOfArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait PluckSelectionOfStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait PluckInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait PluckFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait PluckArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait PluckSelectionOfObjectQuery extends ReqlObject
  trait PluckObjectQuery extends ReqlObject

  implicit class PluckOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def pluck(selectors: ReqlValue*): PluckTableQuery[T] = new PluckTableQuery[T] {
      val command = TermType.PLUCK
      val string = "pluck"
      val arguments = table :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class PluckOnTableSliceOp[T <: ReqlObject, PK](val tableSlice: ReqlTableSlice[T, PK]) {
    def pluck(selectors: ReqlValue*): PluckTableSliceQuery[T] = new PluckTableSliceQuery[T] {
      val command = TermType.PLUCK
      val string = "pluck"
      val arguments = tableSlice :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class PluckOnSelectionOfArrayOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfArray[T, PK]) {
    def pluck(selectors: ReqlValue*): PluckSelectionOfArrayQuery[T] = new PluckSelectionOfArrayQuery[T] {
      val command = TermType.PLUCK
      val string = "pluck"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class PluckOnSelectionOfStreamOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfStream[T, PK]) {
    def pluck(selectors: ReqlValue*): PluckSelectionOfStreamQuery[T] = new PluckSelectionOfStreamQuery[T] {
      val command = TermType.PLUCK
      val string = "pluck"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class PluckOnInfiniteStreamOp[T <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[T]) {
    def pluck(selectors: ReqlValue*): PluckInfiniteStreamQuery[T] = new PluckInfiniteStreamQuery[T] {
      val command = TermType.PLUCK
      val string = "pluck"
      val arguments = infiniteStream :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class PluckOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def pluck(selectors: ReqlValue*): PluckFiniteStreamQuery[T] = new PluckFiniteStreamQuery[T] {
      val command = TermType.PLUCK
      val string = "pluck"
      val arguments = finiteStream :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class PluckOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def pluck(selectors: ReqlValue*): PluckArrayQuery[T] = new PluckArrayQuery[T] {
      val command = TermType.PLUCK
      val string = "pluck"
      val arguments = array :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class PluckOnSelectionOfObjectOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfObject[T, PK]) {
    def pluck(selectors: ReqlValue*): PluckSelectionOfObjectQuery = new PluckSelectionOfObjectQuery {
      val command = TermType.PLUCK
      val string = "pluck"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class PluckOnObjectOp(val obj: ReqlObject) {
    def pluck(selectors: ReqlValue*): PluckObjectQuery = new PluckObjectQuery {
      val command = TermType.PLUCK
      val string = "pluck"
      val arguments = obj :: selectors.toList
      val options = Options.empty
    }
  }

  // without
  trait WithoutTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait WithoutTableSliceQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait WithoutSelectionOfArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait WithoutSelectionOfStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait WithoutInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait WithoutFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait WithoutArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait WithoutSelectionOfObjectQuery extends ReqlObject
  trait WithoutObjectQuery extends ReqlObject

  implicit class WithoutOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def without(selectors: ReqlValue*): WithoutTableQuery[T] = new WithoutTableQuery[T] {
      val command = TermType.WITHOUT
      val string = "without"
      val arguments = table :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithoutOnTableSliceOp[T <: ReqlObject, PK](val tableSlice: ReqlTableSlice[T, PK]) {
    def without(selectors: ReqlValue*): WithoutTableSliceQuery[T] = new WithoutTableSliceQuery[T] {
      val command = TermType.WITHOUT
      val string = "without"
      val arguments = tableSlice :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithoutOnSelectionOfArrayOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfArray[T, PK]) {
    def without(selectors: ReqlValue*): WithoutSelectionOfArrayQuery[T] = new WithoutSelectionOfArrayQuery[T] {
      val command = TermType.WITHOUT
      val string = "without"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithoutOnSelectionOfStreamOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfStream[T, PK]) {
    def without(selectors: ReqlValue*): WithoutSelectionOfStreamQuery[T] = new WithoutSelectionOfStreamQuery[T] {
      val command = TermType.WITHOUT
      val string = "without"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithoutOnInfiniteStreamOp[T <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[T]) {
    def without(selectors: ReqlValue*): WithoutInfiniteStreamQuery[T] = new WithoutInfiniteStreamQuery[T] {
      val command = TermType.WITHOUT
      val string = "without"
      val arguments = infiniteStream :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithoutOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def without(selectors: ReqlValue*): WithoutFiniteStreamQuery[T] = new WithoutFiniteStreamQuery[T] {
      val command = TermType.WITHOUT
      val string = "without"
      val arguments = finiteStream :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithoutOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def without(selectors: ReqlValue*): WithoutArrayQuery[T] = new WithoutArrayQuery[T] {
      val command = TermType.WITHOUT
      val string = "without"
      val arguments = array :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithoutOnSelectionOfObjectOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfObject[T, PK]) {
    def without(selectors: ReqlValue*): WithoutSelectionOfObjectQuery = new WithoutSelectionOfObjectQuery {
      val command = TermType.WITHOUT
      val string = "without"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }
  }

  implicit class WithoutOnObjectOp(val obj: ReqlObject) {
    def without(selectors: ReqlValue*): WithoutObjectQuery = new WithoutObjectQuery {
      val command = TermType.WITHOUT
      val string = "without"
      val arguments = obj :: selectors.toList
      val options = Options.empty
    }
  }

  // merge
  trait MergeTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait MergeTableSliceQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait MergeSelectionOfArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait MergeSelectionOfStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait MergeInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait MergeFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait MergeArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait MergeSelectionOfObjectQuery extends ReqlObject
  trait MergeObjectQuery extends ReqlObject
  //TODO: JsonObject + JsonObject = JsonObject?

  implicit class MergeOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def merge[U <: ReqlObject](objectProducers: ReqlObjectProducer[T, U]*): MergeTableQuery[T] = new MergeTableQuery[T] {
      val command = TermType.MERGE
      val string = "merge"
      val arguments = table :: objectProducers.toList
      val options = Options.empty
    }
  }

  implicit class MergeOnTableSliceOp[T <: ReqlObject, PK](val tableSlice: ReqlTableSlice[T, PK]) {
    def merge[U <: ReqlObject](objectProducers: ReqlObjectProducer[T, U]*): MergeTableSliceQuery[T] = new MergeTableSliceQuery[T] {
      val command = TermType.MERGE
      val string = "merge"
      val arguments = tableSlice :: objectProducers.toList
      val options = Options.empty
    }
  }

  implicit class MergeOnSelectionOfArrayOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfArray[T, PK]) {
    def merge[U <: ReqlObject](objectProducers: ReqlObjectProducer[T, U]*): MergeSelectionOfArrayQuery[T] = new MergeSelectionOfArrayQuery[T] {
      val command = TermType.MERGE
      val string = "merge"
      val arguments = sel :: objectProducers.toList
      val options = Options.empty
    }
  }

  implicit class MergeOnSelectionOfStreamOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfStream[T, PK]) {
    def merge[U <: ReqlObject](objectProducers: ReqlObjectProducer[T, U]*): MergeSelectionOfStreamQuery[T] = new MergeSelectionOfStreamQuery[T] {
      val command = TermType.MERGE
      val string = "merge"
      val arguments = sel :: objectProducers.toList
      val options = Options.empty
    }
  }

  implicit class MergeOnInfiniteStreamOp[T <: ReqlObject](val infiniteStream: ReqlInfiniteStream[T]) {
    def merge[U <: ReqlObject](objectProducers: ReqlObjectProducer[T, U]*): MergeInfiniteStreamQuery[T] = new MergeInfiniteStreamQuery[T] {
      val command = TermType.MERGE
      val string = "merge"
      val arguments = infiniteStream :: objectProducers.toList
      val options = Options.empty
    }
  }

  implicit class MergeOnFiniteStreamOp[T <: ReqlObject](val finiteStream: ReqlFiniteStream[T]) {
    def merge[Type <: ReqlObject](objectProducers: ReqlObjectProducer[T, Type]*): MergeFiniteStreamQuery[Type] = new MergeFiniteStreamQuery[Type] {
      val command = TermType.MERGE
      val string = "merge"
      val arguments = finiteStream :: objectProducers.toList
      val options = Options.empty
    }
  }

  implicit class MergeOnArrayOp[T <: ReqlObject](val array: ReqlArray[T]) {
    def merge[U <: ReqlObject](objectProducers: ReqlObjectProducer[T, U]*): MergeArrayQuery[T] = new MergeArrayQuery[T] {
      val command = TermType.MERGE
      val string = "merge"
      val arguments = array :: objectProducers.toList
      val options = Options.empty
    }
  }

  implicit class MergeOnSelectionOfObjectOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfObject[T, PK]) {
    def merge[U <: ReqlObject](objectProducers: ReqlObjectProducer[T, U]*): MergeSelectionOfObjectQuery = new MergeSelectionOfObjectQuery {
      val command = TermType.MERGE
      val string = "merge"
      val arguments = sel :: objectProducers.toList
      val options = Options.empty
    }
  }

  implicit class MergeOnObjectOp[T <: ReqlObject](val obj: T) {
    def merge[U <: ReqlObject](objectProducers: ReqlObjectProducer[T, U]*): MergeObjectQuery = new MergeObjectQuery {
      val command = TermType.MERGE
      val string = "merge"
      val arguments = obj :: objectProducers.toList
      val options = Options.empty
    }
  }

  // bracket ()
  trait BracketTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait BracketTableSliceQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait BracketSelectionOfArrayWithAttrQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait BracketSelectionOfArrayWithIndexQuery[T <: ReqlObject, PK] extends ReqlSelectionOfObject[T, PK]
  trait BracketSelectionOfStreamWithAttrQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait BracketSelectionOfStreamWithIndexQuery[T <: ReqlObject, PK] extends ReqlSelectionOfObject[T, PK]
  trait BracketInfiniteStreamWithAttrQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait BracketFiniteStreamWithAttrQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait BracketFiniteStreamWithIndexQuery extends ReqlDatum
  trait BracketSelectionOfObjectQuery extends ReqlDatum
  trait BracketObjectQuery extends ReqlDatum
  trait BracketArrayQuery extends ReqlDatum

  /*
   * TODO: maybe move bracket query to separate package (not all._) because not work fine on queries with
   *       implicit parameters (eg .union)
   */
  implicit class BracketOnTableOp[DocType <: ReqlObject, PK](val table: ReqlTable[DocType, PK]) {
    def apply[FieldType <: ReqlDatum](attr: ReqlString): BracketTableQuery[FieldType] = new BracketTableQuery[FieldType] {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = table :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class BracketOnTableSliceOp[DocType <: ReqlObject, PK](val tableSlice: ReqlTableSlice[DocType, PK]) {
    def apply[FieldType <: ReqlDatum](attr: ReqlString): BracketTableSliceQuery[FieldType] = new BracketTableSliceQuery[FieldType] {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = tableSlice :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class BracketOnSelectionOfArrayOp[DocType <: ReqlObject, PK](val sel: ReqlSelectionOfArray[DocType, PK]) {
    def apply[FieldType <: ReqlDatum](attr: ReqlString): BracketSelectionOfArrayWithAttrQuery[FieldType] = new BracketSelectionOfArrayWithAttrQuery[FieldType] {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = sel :: attr :: Nil
      val options = Options.empty
    }

    def apply[FieldType <: ReqlObject](index: ReqlInteger): BracketSelectionOfArrayWithIndexQuery[FieldType, PK] = new BracketSelectionOfArrayWithIndexQuery[FieldType, PK] {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = sel :: index :: Nil
      val options = Options.empty
    }
  }

  implicit class BracketOnSelectionOfStreamOp[DocType <: ReqlObject, PK](val sel: ReqlSelectionOfStream[DocType, PK]) {
    def apply[FieldType <: ReqlDatum](attr: ReqlString): BracketSelectionOfStreamWithAttrQuery[FieldType] = new BracketSelectionOfStreamWithAttrQuery[FieldType] {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = sel :: attr :: Nil
      val options = Options.empty
    }

    def apply[FieldType <: ReqlObject](index: ReqlInteger): BracketSelectionOfStreamWithIndexQuery[FieldType, PK] = new BracketSelectionOfStreamWithIndexQuery[FieldType, PK] {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = sel :: index :: Nil
      val options = Options.empty
    }
  }

  implicit class BracketOnInfiniteStreamOp[DocType <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[DocType]) {
    def apply[FieldType <: ReqlDatum](attr: ReqlString): BracketInfiniteStreamWithAttrQuery[FieldType] = new BracketInfiniteStreamWithAttrQuery[FieldType] {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = infiniteStream :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class BracketOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def apply[AttrType <: ReqlDatum](attr: ReqlString): BracketFiniteStreamWithAttrQuery[AttrType] = new BracketFiniteStreamWithAttrQuery[AttrType] {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = finiteStream :: attr :: Nil
      val options = Options.empty
    }

    def apply(index: ReqlInteger): BracketFiniteStreamWithIndexQuery = new BracketFiniteStreamWithIndexQuery {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = finiteStream :: index :: Nil
      val options = Options.empty
    }
  }

  implicit class BracketOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def apply(index: ReqlNumber): BracketArrayQuery = new BracketArrayQuery {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = array :: index :: Nil
      val options = Options.empty
    }
  }

  implicit class BracketOnSelectionOfObjectOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfObject[T, PK]) {
    def apply(attr: ReqlString): BracketSelectionOfObjectQuery = new BracketSelectionOfObjectQuery {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = sel :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class BracketOnObjectOp(val obj: ReqlObject) {
    def apply(attr: ReqlString): BracketObjectQuery = new BracketObjectQuery {
      val command = TermType.BRACKET
      val string = "bracket"
      val arguments = obj :: attr :: Nil
      val options = Options.empty
    }
  }

  // get_field
  trait GetFieldTableQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait GetFieldTableSliceQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait GetFieldSelectionOfArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait GetFieldSelectionOfStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait GetFiledInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait GetFiledFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait GetFieldSelectionOfObjectQuery[T] extends ReqlDatum
  trait GetFieldObjectQuery extends ReqlDatum

  implicit class GetFieldOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def getField[FieldType <: ReqlDatum](attr: ReqlString): GetFieldTableQuery[FieldType] = new GetFieldTableQuery[FieldType] {
      val command = TermType.GET_FIELD
      val string = "get_field"
      val arguments = table :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class GetFieldOnTableSliceOp[T <: ReqlObject, PK](val tableSlice: ReqlTableSlice[T, PK]) {
    def getField[FieldType <: ReqlDatum](attr: ReqlString): GetFieldTableSliceQuery[FieldType] = new GetFieldTableSliceQuery[FieldType] {
      val command = TermType.GET_FIELD
      val string = "get_field"
      val arguments = tableSlice :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class GetFieldOnSelectionOfArrayOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfArray[T, PK]) {
    def getField[FieldType <: ReqlDatum](attr: ReqlString): GetFieldSelectionOfArrayQuery[FieldType] = new GetFieldSelectionOfArrayQuery[FieldType] {
      val command = TermType.GET_FIELD
      val string = "get_field"
      val arguments = sel :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class GetFieldOnSelectionOfStreamOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfStream[T, PK]) {
    def getField[FieldType <: ReqlDatum](attr: ReqlString): GetFieldSelectionOfStreamQuery[FieldType] = new GetFieldSelectionOfStreamQuery[FieldType] {
      val command = TermType.GET_FIELD
      val string = "get_field"
      val arguments = sel :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class GetFieldOnInfiniteStreamOp[T <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[T]) {
    def getField(attr: ReqlString): GetFiledInfiniteStreamQuery[T] = new GetFiledInfiniteStreamQuery[T] {
      val command = TermType.GET_FIELD
      val string = "get_field"
      val arguments = infiniteStream :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class GetFieldOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def getField[FieldType <: ReqlDatum](attr: ReqlString): GetFiledFiniteStreamQuery[FieldType] = new GetFiledFiniteStreamQuery[FieldType] {
      val command = TermType.GET_FIELD
      val string = "get_field"
      val arguments = finiteStream :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class GetFieldOnSelectionOfObjectOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfObject[T, PK]) {
    def getField[FieldType](attr: ReqlString): GetFieldSelectionOfObjectQuery[FieldType] = new GetFieldSelectionOfObjectQuery[FieldType] {
      val command = TermType.GET_FIELD
      val string = "get_field"
      val arguments = sel :: attr :: Nil
      val options = Options.empty
    }
  }

  implicit class GetFieldOnObjectOp(val obj: ReqlObject) {
    def getField(attr: ReqlString): GetFieldObjectQuery = new GetFieldObjectQuery {
      val command = TermType.GET_FIELD
      val string = "get_field"
      val arguments = obj :: attr :: Nil
      val options = Options.empty
    }
  }

  // has_fields
  trait HasFieldsTableQuery[T <: ReqlObject, PK] extends ReqlSelectionOfStream[T, PK]
  trait HasFieldsTableSliceQuery[T <: ReqlObject, PK] extends ReqlSelectionOfStream[T, PK]
  trait HasFieldsSelectionOfArrayQuery[T <: ReqlObject, PK] extends ReqlSelectionOfArray[T, PK]
  trait HasFieldsSelectionOfStreamQuery[T <: ReqlObject, PK] extends ReqlSelectionOfStream[T, PK]
  trait HasFieldsInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait HasFieldsFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait HasFieldsArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  trait HasFieldsSingleSelectionQuery extends ReqlBoolean
  trait HasFieldsObjectQuery extends ReqlBoolean

  //TODO: more safety for selector - string, array[string], jsonObject
  implicit class HasFieldsOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def hasFields(selectors: ReqlValue*): HasFieldsTableQuery[T, PK] = new HasFieldsTableQuery[T, PK] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = table :: selectors.toList
      val options = Options.empty
    }

    def hasFields(args: ReqlArgs): HasFieldsTableQuery[T, PK] = new HasFieldsTableQuery[T, PK] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = table :: args :: Nil
      val options = Options.empty
    }
  }

  implicit class HasFieldsOnTableSliceOp[T <: ReqlObject, PK](val tableSlice: ReqlTableSlice[T, PK]) {
    def hasFields(selectors: ReqlValue*): HasFieldsTableSliceQuery[T, PK] = new HasFieldsTableSliceQuery[T, PK] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = tableSlice :: selectors.toList
      val options = Options.empty
    }

    def hasFields(args: ReqlArgs): HasFieldsTableSliceQuery[T, PK] = new HasFieldsTableSliceQuery[T, PK] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = tableSlice :: args :: Nil
      val options = Options.empty
    }
  }

  implicit class HasFieldsOnSelectionOfArrayOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfArray[T, PK]) {
    def hasFields(selectors: ReqlValue*): HasFieldsSelectionOfArrayQuery[T, PK] = new HasFieldsSelectionOfArrayQuery[T, PK] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }

    def hasFields(args: ReqlArgs): HasFieldsSelectionOfArrayQuery[T, PK] = new HasFieldsSelectionOfArrayQuery[T, PK] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = sel :: args :: Nil
      val options = Options.empty
    }
  }

  implicit class HasFieldsOnSelectionOfStreamOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfStream[T, PK]) {
    def hasFields(selectors: ReqlValue*): HasFieldsSelectionOfStreamQuery[T, PK] = new HasFieldsSelectionOfStreamQuery[T, PK] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = sel :: selectors.toList
      val options = Options.empty
    }

    def hasFields(args: ReqlArgs): HasFieldsSelectionOfStreamQuery[T, PK] = new HasFieldsSelectionOfStreamQuery[T, PK] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = sel :: args :: Nil
      val options = Options.empty
    }
  }

  implicit class HasFieldsOnInfiniteStreamOp[T <: ReqlDatum](val infiniteStream: ReqlInfiniteStream[T]) {
    def hasFields(selectors: ReqlValue*): HasFieldsInfiniteStreamQuery[T] = new HasFieldsInfiniteStreamQuery[T] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = infiniteStream :: selectors.toList
      val options = Options.empty
    }

    def hasFields(args: ReqlArgs): HasFieldsInfiniteStreamQuery[T] = new HasFieldsInfiniteStreamQuery[T] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = infiniteStream :: args :: Nil
      val options = Options.empty
    }
  }

  implicit class HasFieldsOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def hasFields(selectors: ReqlValue*): HasFieldsFiniteStreamQuery[T] = new HasFieldsFiniteStreamQuery[T] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = finiteStream :: selectors.toList
      val options = Options.empty
    }

    def hasFields(args: ReqlArgs): HasFieldsFiniteStreamQuery[T] = new HasFieldsFiniteStreamQuery[T] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = finiteStream :: args :: Nil
      val options = Options.empty
    }
  }

  implicit class HasFieldsOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def hasFields(selectors: ReqlValue*): HasFieldsArrayQuery[T] = new HasFieldsArrayQuery[T] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = array :: selectors.toList
      val options = Options.empty
    }

    def hasFields(args: ReqlArgs): HasFieldsArrayQuery[T] = new HasFieldsArrayQuery[T] {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = array :: args :: Nil
      val options = Options.empty
    }
  }

  implicit class HasFieldsOnSingleSelectionOp[T <: ReqlObject, PK](val singleSelection: ReqlSelectionOfObject[T, PK]) {
    def hasFields(selectors: ReqlValue*): HasFieldsSingleSelectionQuery = new HasFieldsSingleSelectionQuery {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = singleSelection :: selectors.toList
      val options = Options.empty
    }

    def hasFields(args: ReqlArgs): HasFieldsSingleSelectionQuery = new HasFieldsSingleSelectionQuery {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = singleSelection :: args :: Nil
      val options = Options.empty
    }
  }

  implicit class HasFieldsOnObjectOp(val obj: ReqlObject) {
    def hasFields(selectors: ReqlValue*): HasFieldsObjectQuery = new HasFieldsObjectQuery {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = obj :: selectors.toList
      val options = Options.empty
    }

    def hasFields(args: ReqlArgs): HasFieldsObjectQuery = new HasFieldsObjectQuery {
      val command = TermType.HAS_FIELDS
      val string = "has_fields"
      val arguments = obj :: args :: Nil
      val options = Options.empty
    }
  }

  // keys
  trait KeysSelectionOfObjectQuery extends ReqlArray[ReqlString]
  trait KeysObjectQuery extends ReqlArray[ReqlString]

  implicit class KeysOnSelectionOfObjectOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfObject[T, PK]) {
    def keys(): KeysSelectionOfObjectQuery = new KeysSelectionOfObjectQuery {
      val command = TermType.KEYS
      val string = "keys"
      val arguments = sel :: Nil
      val options = Options.empty
    }
  }

  implicit class KeysOnObjectOp(val obj: ReqlObject) {
    def keys(): KeysObjectQuery = new KeysObjectQuery {
      val command = TermType.KEYS
      val string = "keys"
      val arguments = obj :: Nil
      val options = Options.empty
    }
  }

  // values
  abstract class ValuesSelectionOfObjectQuery[T <: ReqlDatum] extends ReqlArray[T]
  abstract class ValuesObjectQuery[T <: ReqlDatum] extends ReqlArray[T]
  //TODO: check inferred types

  implicit class ValuesOnSelectionOfObjectOp[T <: ReqlObject, PK](val sel: ReqlSelectionOfObject[T, PK]) {
    def values[FieldTypeLub <: ReqlDatum](): ValuesSelectionOfObjectQuery[FieldTypeLub] = new ValuesSelectionOfObjectQuery[FieldTypeLub] {
      val command = TermType.VALUES
      val string = "values"
      val arguments = sel :: Nil
      val options = Options.empty
    }
  }

  implicit class ValuesOnObjectOp(val obj: ReqlObject) {
    def values[FieldTypeLub <: ReqlDatum](): ValuesObjectQuery[FieldTypeLub] = new ValuesObjectQuery[FieldTypeLub] {
      val command = TermType.VALUES
      val string = "values"
      val arguments = obj :: Nil
      val options = Options.empty
    }
  }

  // literal
  trait LiteralQuery extends ReqlLiteral

  implicit class LiteralOnROp(val r: ReqlR) {
    def literal(obj: ReqlObject): LiteralQuery = new LiteralQuery {
      val command = TermType.LITERAL
      val string = "literal"
      val arguments = obj :: Nil
      val options = Options.empty
    }

    def literal(): LiteralQuery = new LiteralQuery {
      val command = TermType.LITERAL
      val string = "literal"
      val arguments = Nil
      val options = Options.empty
    }
  }

  // object
  trait ObjectConstructorQuery extends ReqlObject

  implicit class ObjectOnROp(val r: ReqlR) {
    def obj(pairs: (ReqlString, ReqlDatum)*): ObjectConstructorQuery = new ObjectConstructorQuery {
      val command = TermType.OBJECT
      val string = "object"
      val arguments = pairs.flatMap { case (key, value) => key :: value :: Nil }.toList
      val options = Options.empty
    }
  }

}
