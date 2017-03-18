package rere.ql.values

import io.circe.Json
import rere.ql.types.{ReqlBoolean, ReqlPrimitiveExpr}

class ReqlBooleanQuery(boolean: Boolean) extends ReqlBoolean with ReqlPrimitiveExpr {
  def string = "bool"
  def repr = Json.fromBoolean(boolean).noSpaces
}
