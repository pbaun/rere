package rere.ql.typeclasses

import rere.ql.shapes.ReqlModel
import rere.ql.types._
import rere.ql.util._

import scala.annotation.implicitNotFound

/**
  * Firstly created for .do query to implicitly cast results to function output type.
  * Typically used for converting any type to some particular type
  *
  * @tparam To - transmuter output type
  */
@implicitNotFound("Type ${To} can't be used as argument of lambda")
trait Transmuter[To] {
  def transmute(query: ReqlExpr): To
}

object Transmuter extends LowPriorityTransmuter {

  def transmute[T](query: ReqlExpr)(implicit toT: Transmuter[T]): T = toT.transmute(query)

  implicit val nullTransmuter: Transmuter[ReqlNull] = new Transmuter[ReqlNull] {
    def transmute(query: ReqlExpr): ReqlNull = new NullHintProxy(query)
  }

  implicit val booleanTransmuter: Transmuter[ReqlBoolean] = new Transmuter[ReqlBoolean] {
    def transmute(query: ReqlExpr): ReqlBoolean = new BooleanHintProxy(query)
  }

  implicit val integerTransmuter: Transmuter[ReqlInteger] = new Transmuter[ReqlInteger] {
    def transmute(query: ReqlExpr): ReqlInteger = new IntegerHintProxy(query)
  }

  implicit val floatTransmuter: Transmuter[ReqlFloat] = new Transmuter[ReqlFloat] {
    def transmute(query: ReqlExpr): ReqlFloat = new FloatHintProxy(query)
  }

  implicit val stringTransmuter: Transmuter[ReqlString] = new Transmuter[ReqlString] {
    def transmute(query: ReqlExpr): ReqlString = new StringHintProxy(query)
  }

  implicit def arrayTransmuter[T <: ReqlDatum]: Transmuter[ReqlArray[T]] = new Transmuter[ReqlArray[T]] {
    def transmute(query: ReqlExpr): ReqlArray[T] = new ArrayHintProxy[T](query)
  }

  implicit val objectTransmuter: Transmuter[ReqlObject] = new Transmuter[ReqlObject] {
    def transmute(query: ReqlExpr): ReqlObject = new ObjectHintProxy(query)
  }

  implicit val jsonTransmuter: Transmuter[ReqlJson] = new Transmuter[ReqlJson] {
    def transmute(query: ReqlExpr): ReqlJson = new JsonHintProxy(query)
  }

  implicit val jsonObjectTransmuter: Transmuter[ReqlJsonObject] = new Transmuter[ReqlJsonObject] {
    def transmute(query: ReqlExpr): ReqlJsonObject = new JsonObjectHintProxy(query)
  }

  //
  // This types can be used as return type of lambda inside .do query
  //
  implicit def tableTransmuter[T <: ReqlObject]: Transmuter[ReqlTable[T]] = new Transmuter[ReqlTable[T]] {
    def transmute(query: ReqlExpr): ReqlTable[T] = new ProxyQuery(query) with ReqlTable[T]
  }

  implicit def tableSliceTransmuter[T <: ReqlObject]: Transmuter[ReqlTableSlice[T]] = new Transmuter[ReqlTableSlice[T]] {
    def transmute(query: ReqlExpr): ReqlTableSlice[T] = new ProxyQuery(query) with ReqlTableSlice[T]
  }

  implicit def selectionOfArrayTransmuter[T <: ReqlObject]: Transmuter[ReqlSelectionOfArray[T]] = new Transmuter[ReqlSelectionOfArray[T]] {
    def transmute(query: ReqlExpr): ReqlSelectionOfArray[T] = new ProxyQuery(query) with ReqlSelectionOfArray[T]
  }

  implicit def selectionOfStreamTransmuter[T <: ReqlObject]: Transmuter[ReqlSelectionOfStream[T]] = new Transmuter[ReqlSelectionOfStream[T]] {
    def transmute(query: ReqlExpr): ReqlSelectionOfStream[T] = new ProxyQuery(query) with ReqlSelectionOfStream[T]
  }

  implicit def selectionOfObjectTransmuter[T <: ReqlObject]: Transmuter[ReqlSelectionOfObject[T]] = new Transmuter[ReqlSelectionOfObject[T]] {
    def transmute(query: ReqlExpr): ReqlSelectionOfObject[T] = new ProxyQuery(query) with ReqlSelectionOfObject[T]
  }

  implicit def finiteStreamTransmuter[T <: ReqlDatum]: Transmuter[ReqlFiniteStream[T]] = new Transmuter[ReqlFiniteStream[T]] {
    def transmute(query: ReqlExpr): ReqlFiniteStream[T] = new ProxyQuery(query) with ReqlFiniteStream[T]
  }

  implicit def infiniteStreamTransmuter[T <: ReqlDatum]: Transmuter[ReqlInfiniteStream[T]] = new Transmuter[ReqlInfiniteStream[T]] {
    def transmute(query: ReqlExpr): ReqlInfiniteStream[T] = new ProxyQuery(query) with ReqlInfiniteStream[T]
  }

  implicit val binaryTransmuter: Transmuter[ReqlBinary] = new Transmuter[ReqlBinary] {
    def transmute(query: ReqlExpr): ReqlBinary = new BinaryHintProxy(query)
  }

  implicit val timeTransmuter: Transmuter[ReqlTime] = new Transmuter[ReqlTime] {
    def transmute(query: ReqlExpr): ReqlTime = new ProxyQuery(query) with ReqlTime
  }

  implicit def modelTransmuter[T]: Transmuter[ReqlModel[T]] = new Transmuter[ReqlModel[T]] {
    def transmute(query: ReqlExpr): ReqlModel[T] = new ModelHintProxy[T](query)
  }
}

trait LowPriorityTransmuter {
  implicit val datumTransmuter: Transmuter[ReqlDatum] = new Transmuter[ReqlDatum] {
    def transmute(query: ReqlExpr): ReqlDatum = new ProxyQuery(query) with ReqlDatum
  }

  implicit val numberTransmuter: Transmuter[ReqlNumber] = new Transmuter[ReqlNumber] {
    def transmute(query: ReqlExpr): ReqlNumber = new NumberHintProxy(query)
  }

  //TODO: fixit; it should work using object transmuter, separate instances should not be used
  implicit def changefeedNotificationTransmuter[T <: ReqlDatum]: Transmuter[ReqlChangefeedNotification[T]] = new Transmuter[ReqlChangefeedNotification[T]] {
    def transmute(query: ReqlExpr): ReqlChangefeedNotification[T] = new ProxyQuery(query) with ReqlChangefeedNotification[T]
  }
}