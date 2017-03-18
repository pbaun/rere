package rere.ql.values

import io.circe.Json
import rere.ql.types.{ReqlFloat, ReqlPrimitiveExpr}

//TODO: make notice in documentation about values that cannot be represented as a JSON number
class ReqlDoubleQuery(double: Double) extends ReqlFloat with ReqlPrimitiveExpr {
  def string = "number"
  def repr = Json.fromDoubleOrNull(double).noSpaces
}
