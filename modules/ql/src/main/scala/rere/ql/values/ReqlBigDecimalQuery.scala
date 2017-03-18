package rere.ql.values

import io.circe.Json
import rere.ql.types.{ReqlFloat, ReqlPrimitiveExpr}

class ReqlBigDecimalQuery(bigDecimal: BigDecimal) extends ReqlFloat with ReqlPrimitiveExpr {
  def string = "number"
  def repr = Json.fromBigDecimal(bigDecimal).noSpaces
}
