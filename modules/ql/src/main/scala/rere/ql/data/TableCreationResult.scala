package rere.ql.data

case class TableCreationResult(
  tablesCreated: Long,
  configChanges: Seq[ChangefeedNotification[TableConfig]]
)
