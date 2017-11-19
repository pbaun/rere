package rere.ql.queries

import rere.ql.typeclasses.{ToPredicate, Transmuter}
import rere.ql.types._

trait AllQueries
  extends RQuery
  with StructuresDSL
  with ValueQueries
  with BinaryQueries
  with LogicQueries
  with MathQueries
  with DatabaseQueries
  with TableQueries
  with WritingQueries
  with SelectingQueries
  with JoinQueries
  with TransformationQueries
  with AggregationQueries
  with DocumentQueries
  with ArrayQueries
  with StringQueries
  with DateQueries
  with ControlQueries
  with GeospatialQueries
  with AdministrationQueries
  with ChangesQueries {

  //TODO: move it somewhere (TopLevelQueries?)
  implicit class ExprOp(r: ReqlR) extends ValueQueries

  //TODO: it's extension, not part of official api
  implicit class PredicateOp(val r: ReqlR) {
    def pred[T <: ReqlDatum : Transmuter](datum: T): ReqlPredicate[T] = {
      ToPredicate(datum)
    }

    def pred[T <: ReqlDatum : Transmuter](function: T => ReqlBoolean): ReqlPredicate[T] = {
      ToPredicate(function)
    }
  }

  //TODO: it's extension, not part of official api
  implicit class SelectorOp(val r: ReqlR) {
    def sel[T <: ReqlObject, U <: ReqlDatum](str: String): ReqlDatumSelector[T, U] = {
      str
    }
  }

  //TODO: it's extension, not part of official api
  implicit class TypeHintOnBasicOp(val datum: ReqlDatum) {
    import rere.ql.util._

    def asNull: ReqlNull = new NullHintProxy(datum)
    def asBoolean: ReqlBoolean = new BooleanHintProxy(datum)
    def asInteger: ReqlInteger = new IntegerHintProxy(datum)
    def asFloat: ReqlFloat = new FloatHintProxy(datum)
    def asString: ReqlString = new StringHintProxy(datum)
    def asArray: ReqlArray[Nothing] = new ArrayHintProxy[Nothing](datum)
    def asArrayOf[T <: ReqlDatum]: ReqlArray[T] = new ArrayHintProxy[T](datum)
    def asObject: ReqlObject = new ObjectHintProxy(datum)
    def asBinary: ReqlBinary = new BinaryHintProxy(datum)
  }

}