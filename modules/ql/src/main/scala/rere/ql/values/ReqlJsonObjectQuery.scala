package rere.ql.values

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import rere.ql.types.{ReqlJsonObject, ReqlPrimitiveExpr}
import rere.ql.util.JsonToReql

class ReqlJsonObjectQuery(jsonObj: JsonObject)
  extends ReqlJsonObject with ReqlPrimitiveExpr {

  def string = "make_obj"
  def repr = JsonToReql.transform(Json.fromJsonObject(jsonObj)).asJson.noSpaces

  override def toString: String = s"ReqlJsonObjectQuery($jsonObj)"
}
