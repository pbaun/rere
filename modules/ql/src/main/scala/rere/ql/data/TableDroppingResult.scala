package rere.ql.data

case class TableDroppingResult(
  tablesDropped: Long,
  configChanges: Seq[ChangefeedNotification[TableConfig]]
)
