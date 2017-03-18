package rere.ql.data

case class ReconfiguringResult(
  reconfigured: Long,
  configChanges: Seq[ChangefeedNotification[TableConfig]],
  statusChanges: Seq[ChangefeedNotification[TableStatus]]
)

case class ReconfiguringDryResult(
  reconfigured: Long,
  configChanges: Seq[ChangefeedNotification[TableConfig]]
)