package rere.ql.data

import rere.ql.queries.values
import rere.ql.types.{ReqlDatum, ReqlUserPermissions}
import rere.ql.values.ReqlMakeObjFromPairsListQuery

case class UserPermissions(
  read: Option[Boolean],
  write: Option[Boolean],
  connect: Option[Boolean],
  config: Option[Boolean]
)

object UserPermissions {
  implicit def toReqlUserPermissions(userPermissions: UserPermissions): ReqlUserPermissions = {
    val permissions: List[(String, ReqlDatum)] =
      userPermissions.read.map(v => "read" -> values.expr(v)).toList ++
      userPermissions.write.map(v => "write" -> values.expr(v)).toList ++
      userPermissions.connect.map(v => "connect" -> values.expr(v)).toList ++
      userPermissions.config.map(v => "config" -> values.expr(v)).toList

    new ReqlMakeObjFromPairsListQuery(permissions) with ReqlUserPermissions
  }
}
