package rere.ql.values

import io.circe.Json
import rere.ql.types.{ReqlPrimitiveExpr, ReqlString}

class ReqlStringQuery(stringValue: String) extends ReqlString with ReqlPrimitiveExpr {
  def string = "str"
  def repr = Json.fromString(stringValue).noSpaces
}
