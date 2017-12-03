package rere.ql.queries

import java.util.UUID

import io.circe.{Json, JsonObject}
import rere.ql.types._
import rere.ql.values._

/**
  * Implicit converters for standard types
  */
trait ValueQueries {

  implicit def expr(nullRef: Null): ReqlNull = new ReqlNullQuery(nullRef)

  implicit def expr(bool: Boolean): ReqlBoolean = new ReqlBooleanQuery(bool)

  implicit def expr(number: Int): ReqlInteger = new ReqlIntQuery(number)
  implicit def expr(number: Long): ReqlInteger = new ReqlLongQuery(number)
  implicit def expr(number: BigInt): ReqlInteger = new ReqlBigIntQuery(number)
  implicit def expr(number: Double): ReqlFloat = new ReqlDoubleQuery(number)
  implicit def expr(number: BigDecimal): ReqlFloat = new ReqlBigDecimalQuery(number)

  implicit def expr(string: String): ReqlString = new ReqlStringQuery(string)

  implicit def expr(uuid: UUID): ReqlUUID = new ReqlUUIDQuery(uuid)

  implicit def expr(jsonArray: List[Json]): ReqlArray[ReqlJson] = new ReqlJsonArrayQuery(jsonArray)
  implicit def expr[T <: ReqlDatum](iterable: Iterable[T]): ReqlArray[T] =
    new ReqlMakeArrayFromIterableQuery(iterable)

  implicit def expr(jsonObj: JsonObject): ReqlJsonObject = new ReqlJsonObjectQuery(jsonObj)
  implicit def expr(reqlObj: Map[String, ReqlDatum]): ReqlObject =
    new ReqlMakeObjFromMapQuery(reqlObj)

  implicit def expr(json: Json): ReqlJson = new ReqlJsonQuery(json)
}
