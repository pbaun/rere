package rere.ql.data

import rere.ql.typeclasses.Transmuter
import rere.ql.types.{ReqlChangefeedNotification, ReqlDatum}
import rere.ql.values.ReqlMakeObjFromPairsListQuery
import rere.ql.wire.ReqlEncoder

case class ChangefeedNotification[T](oldVal: Option[T], newVal: Option[T])

object ChangefeedNotification {
  implicit def toReqlChangefeedNotification[T, Reql <: ReqlDatum](
    changefeedNotification: ChangefeedNotification[T]
  )(
    implicit
    encoder: ReqlEncoder.Aux[T, Reql],
    transmuter: Transmuter[Reql]
  ): ReqlChangefeedNotification[T] = {
    val notification =
      "old_val" -> ReqlEncoder[Option[T]].encode(changefeedNotification.oldVal) ::
      "new_val" -> ReqlEncoder[Option[T]].encode(changefeedNotification.newVal) ::
      Nil

    new ReqlMakeObjFromPairsListQuery(notification) with ReqlChangefeedNotification[T]
  }
}
