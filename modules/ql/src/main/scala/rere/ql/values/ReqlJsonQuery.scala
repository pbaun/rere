package rere.ql.values

import io.circe.Json
import io.circe.syntax.EncoderOps
import rere.ql.types.{ReqlJson, ReqlPrimitiveExpr}
import rere.ql.util.JsonToReql

class ReqlJsonQuery(json: Json)
  extends ReqlJson with ReqlPrimitiveExpr {

  def string = "datum"
  def repr = JsonToReql.transform(json).asJson.noSpaces

  override def toString: String = s"ReqlJsonQuery($json)"
}
