package rere.ql.types

import io.circe.JsonObject
import rere.ql.queries.values
import rere.ql.shapes.{ModelShape, ReqlModel}
import rere.ql.typeclasses.Transmuter
import rere.ql.util.{FunctionProxyQuery, ProxyQuery}

/**
  * Some kind of object, null value of object producing function that can be used in
  * .merge, .update and .replace queries.
  *
  * @tparam SourceType - type of collection or selection
  * @tparam TargetType - type that wrapped producer can produce
  */
trait ReqlObjectProducer[SourceType, TargetType] extends ReqlExpr

//TODO: move this to typeclasses package
object ReqlObjectProducer {

  // NOTE
  // There no instance for Null type because it will not be wrapped by implicit, null already is
  // an instance of any type

  implicit def reqlNullToObjectProducer[
    SourceType <: ReqlDatum,
    TargetType <: ReqlObject
  ](value: ReqlNull): ReqlObjectProducer[SourceType, TargetType] =
    new ProxyQuery(value) with ReqlObjectProducer[SourceType, TargetType]

  implicit def objectToObjectProducer[
    SourceType <: ReqlDatum,
    TargetType <: ReqlObject
  ](value: JsonObject): ReqlObjectProducer[SourceType, TargetType] =
    new ProxyQuery(values.expr(value)) with ReqlObjectProducer[SourceType, TargetType]

  implicit def mapToObjectProducer[
    SourceType <: ReqlDatum,
    TargetType <: ReqlObject
  ](value: Map[String, ReqlDatum]): ReqlObjectProducer[SourceType, TargetType] =
    new ProxyQuery(values.expr(value)) with ReqlObjectProducer[SourceType, TargetType]

  implicit def reqlObjectToObjectProducer[
    SourceType <: ReqlDatum,
    TargetType <: ReqlObject
  ](value: TargetType): ReqlObjectProducer[SourceType, TargetType] =
    new ProxyQuery(value) with ReqlObjectProducer[SourceType, TargetType]

  implicit def reqlModelToObjectProducer[SourceType <: ReqlDatum, T, PK](
    value: T)(
    implicit shape: ModelShape[T, PK]
  ): ReqlObjectProducer[SourceType, ReqlModel[T, PK]] =
    new ProxyQuery(shape.toReqlObject(value)) with ReqlObjectProducer[SourceType, ReqlModel[T, PK]]

  implicit def functionToObjectProducer[
    SourceType <: ReqlDatum : Transmuter,
    TargetType <: ReqlObject
  ](f: SourceType => TargetType): ReqlObjectProducer[SourceType, TargetType] =
    new FunctionProxyQuery[SourceType, TargetType](f) with ReqlObjectProducer[SourceType, TargetType]

}
