package rere.ql.data

case class GrantingResult(
  granted: Long,
  permissionsChanges: Seq[ChangefeedNotification[UserPermissions]]
)
