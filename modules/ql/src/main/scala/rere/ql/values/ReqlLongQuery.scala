package rere.ql.values

import io.circe.Json
import rere.ql.types.{ReqlInteger, ReqlPrimitiveExpr}

class ReqlLongQuery(long: Long) extends ReqlInteger with ReqlPrimitiveExpr {
  def string = "number"
  def repr = Json.fromLong(long).noSpaces
}
