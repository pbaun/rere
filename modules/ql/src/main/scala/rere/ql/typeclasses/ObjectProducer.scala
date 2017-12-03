package rere.ql.typeclasses

import io.circe.JsonObject
import rere.ql.queries.values
import rere.ql.shapes.ModelShape
import rere.ql.types._
import rere.ql.util.{FunctionProxyQuery, ProxyQuery}

/**
  * Some kind of object, null value of object producing function that can be used in
  * .merge, .update and .replace queries.
  *
  * @tparam SourceType - type of collection or selection
  * @tparam TargetType - type that wrapped producer can produce
  */
trait ObjectProducer[SourceType, TargetType] extends ReqlExpr

object ObjectProducer {

  implicit def reqlNullToObjectProducer[
    SourceType <: ReqlDatum,
    TargetType <: ReqlObject
  ](value: ReqlNull): ObjectProducer[SourceType, TargetType] =
    new ProxyQuery(value) with ObjectProducer[SourceType, TargetType]

  implicit def objectToObjectProducer[
    SourceType <: ReqlDatum,
    TargetType <: ReqlObject
  ](value: JsonObject): ObjectProducer[SourceType, TargetType] =
    new ProxyQuery(values.expr(value)) with ObjectProducer[SourceType, TargetType]

  implicit def mapToObjectProducer[
    SourceType <: ReqlDatum,
    TargetType <: ReqlObject
  ](value: Map[String, ReqlDatum]): ObjectProducer[SourceType, TargetType] =
    new ProxyQuery(values.expr(value)) with ObjectProducer[SourceType, TargetType]

  implicit def reqlObjectToObjectProducer[
    SourceType <: ReqlDatum,
    TargetType <: ReqlObject
  ](value: TargetType): ObjectProducer[SourceType, TargetType] =
    new ProxyQuery(value) with ObjectProducer[SourceType, TargetType]

  implicit def reqlModelToObjectProducer[SourceType <: ReqlDatum, T, PK <: PrimaryKey](
    value: T)(
    implicit shape: ModelShape[T, PK]
  ): ObjectProducer[SourceType, ReqlModel[T, PK]] =
    new ProxyQuery(shape.toReqlObject(value)) with ObjectProducer[SourceType, ReqlModel[T, PK]]

  implicit def functionToObjectProducer[
    SourceType <: ReqlDatum : Transmuter,
    TargetType <: ReqlObject
  ](f: SourceType => TargetType): ObjectProducer[SourceType, TargetType] =
    new FunctionProxyQuery[SourceType, TargetType](f) with ObjectProducer[SourceType, TargetType]

}
