package rere.ql.data

import rere.ql.queries.values
import rere.ql.types.{ReqlDatum, ReqlUserPermissions}
import rere.ql.values.ReqlMakeObjFromMapQuery

case class UserPermissions(
  read: Option[Boolean],
  write: Option[Boolean],
  connect: Option[Boolean],
  config: Option[Boolean]
)

object UserPermissions {
  implicit def toReqlUserPermissions(permissions: UserPermissions): ReqlUserPermissions = {
    val permissionsMap: Map[String, ReqlDatum] = (
      permissions.read.map(v => "read" -> values.expr(v)).toList ++
      permissions.write.map(v => "write" -> values.expr(v)).toList ++
      permissions.connect.map(v => "connect" -> values.expr(v)).toList ++
      permissions.config.map(v => "config" -> values.expr(v)).toList
    ).toMap

    new ReqlMakeObjFromMapQuery(permissionsMap) with ReqlUserPermissions
  }
}
