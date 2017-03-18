package rere.ql.values

import io.circe.Json
import rere.ql.types.{ReqlInteger, ReqlPrimitiveExpr}

class ReqlIntQuery(int: Int) extends ReqlInteger with ReqlPrimitiveExpr {
  def string = "number"
  def repr = Json.fromInt(int).noSpaces
}
