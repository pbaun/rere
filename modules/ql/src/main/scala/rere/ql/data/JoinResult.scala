package rere.ql.data

import rere.ql.types.{ReqlDatum, ReqlJoinResult}
import rere.ql.values.ReqlMakeObjFromPairsListQuery
import rere.ql.wire.ReqlEncoder

case class JoinResult[LeftType, RightType](left: LeftType, right: RightType)

object JoinResult {
  implicit def toReqlJsonResult[LeftType : ReqlEncoder, RightType : ReqlEncoder](
    joinResult: JoinResult[LeftType, RightType]
  ): ReqlJoinResult[LeftType, RightType] = {
    val result: List[(String, ReqlDatum)] =
      "left" -> ReqlEncoder[LeftType].encode(joinResult.left) ::
      "right" -> ReqlEncoder[RightType].encode(joinResult.right) ::
      Nil

    new ReqlMakeObjFromPairsListQuery(result) with ReqlJoinResult[LeftType, RightType]
  }
}
