package rere.ql.values

import io.circe.Json
import rere.ql.types.{ReqlNull, ReqlPrimitiveExpr}

class ReqlNullQuery(nullRef: Null) extends ReqlNull with ReqlPrimitiveExpr {
  def string = "null"
  def repr = Json.Null.noSpaces
}
