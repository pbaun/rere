package rere.ql.typeclasses

import rere.ql.shapes.{ModelShape, ReqlModel}
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

  implicit val jsonTransmuter: Transmuter[ReqlJson] = new Transmuter[ReqlJson] {
    def transmute(query: ReqlExpr): ReqlJson = new JsonHintProxy(query)
  }

  //
  // This types can be used as return type of lambda inside .do query
  //
  implicit def tableTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Transmuter[ReqlTable[T, PK]] = new Transmuter[ReqlTable[T, PK]] {
    def transmute(query: ReqlExpr): ReqlTable[T, PK] = new ProxyQuery(query) with ReqlTable[T, PK] {
      override def shape: ModelShape[T, PK] = modelShape
    }
  }

  implicit def tableSliceTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Transmuter[ReqlTableSlice[T, PK]] = new Transmuter[ReqlTableSlice[T, PK]] {
    def transmute(query: ReqlExpr): ReqlTableSlice[T, PK] = new ProxyQuery(query) with ReqlTableSlice[T, PK] {
      override def shape: ModelShape[T, PK] = modelShape
    }
  }

  implicit def selectionOfArrayTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Transmuter[ReqlSelectionOfArray[T, PK]] = new Transmuter[ReqlSelectionOfArray[T, PK]] {
    def transmute(query: ReqlExpr): ReqlSelectionOfArray[T, PK] = new ProxyQuery(query) with ReqlSelectionOfArray[T, PK] {
      override def shape: ModelShape[T, PK] = modelShape
    }
  }

  implicit def selectionOfStreamTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Transmuter[ReqlSelectionOfStream[T, PK]] = new Transmuter[ReqlSelectionOfStream[T, PK]] {
    def transmute(query: ReqlExpr): ReqlSelectionOfStream[T, PK] = new ProxyQuery(query) with ReqlSelectionOfStream[T, PK] {
      override def shape: ModelShape[T, PK] = modelShape
    }
  }

  implicit def selectionOfObjectTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Transmuter[ReqlSelectionOfObject[T, PK]] = new Transmuter[ReqlSelectionOfObject[T, PK]] {
    def transmute(query: ReqlExpr): ReqlSelectionOfObject[T, PK] = new ProxyQuery(query) with ReqlSelectionOfObject[T, PK] {
      override def shape: ModelShape[T, PK] = modelShape
    }
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

  implicit def modelTransmuter[T, PK](implicit shape: ModelShape[T, PK]): Transmuter[ReqlModel[T, PK]] = new Transmuter[ReqlModel[T, PK]] {
    def transmute(query: ReqlExpr): ReqlModel[T, PK] = new ModelHintProxy[T, PK](query, shape)
  }
}

trait LowPriorityTransmuter {
  implicit val objectTransmuter: Transmuter[ReqlObject] = new Transmuter[ReqlObject] {
    def transmute(query: ReqlExpr): ReqlObject = new ObjectHintProxy(query)
  }

  //TODO: remove it
  implicit val jsonObjectTransmuter: Transmuter[ReqlJsonObject] = new Transmuter[ReqlJsonObject] {
    def transmute(query: ReqlExpr): ReqlJsonObject = new JsonObjectHintProxy(query)
  }

  implicit val datumTransmuter: Transmuter[ReqlDatum] = new Transmuter[ReqlDatum] {
    def transmute(query: ReqlExpr): ReqlDatum = new ProxyQuery(query) with ReqlDatum
  }

  implicit val numberTransmuter: Transmuter[ReqlNumber] = new Transmuter[ReqlNumber] {
    def transmute(query: ReqlExpr): ReqlNumber = new NumberHintProxy(query)
  }

  //TODO: fixit; it should work using object transmuter, separate instances should not be used
  implicit def changefeedNotificationTransmuter[T]: Transmuter[ReqlChangefeedNotification[T]] = new Transmuter[ReqlChangefeedNotification[T]] {
    def transmute(query: ReqlExpr): ReqlChangefeedNotification[T] = new ProxyQuery(query) with ReqlChangefeedNotification[T]
  }
}