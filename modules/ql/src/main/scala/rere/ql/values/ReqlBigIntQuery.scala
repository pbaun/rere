package rere.ql.values

import io.circe.Json
import rere.ql.types.{ReqlInteger, ReqlPrimitiveExpr}

class ReqlBigIntQuery(bigInt: BigInt) extends ReqlInteger with ReqlPrimitiveExpr {
  def string = "number"
  def repr = Json.fromBigInt(bigInt).noSpaces
}
