package rere.ql.values

import io.circe.Json
import io.circe.syntax.EncoderOps
import rere.ql.types.{ReqlArray, ReqlDatum, ReqlJson, ReqlPrimitiveExpr}
import rere.ql.util.JsonToReql

class ReqlJsonArrayQuery[T <: ReqlDatum](jsonArray: Iterable[Json])
  extends ReqlJson with ReqlArray[T] with ReqlPrimitiveExpr {

  def string = "make_array"
  def repr = JsonToReql.transform(Json.fromValues(jsonArray)).asJson.noSpaces

  override def toString: String = s"ReqlJsonArrayQuery($jsonArray)"
}
