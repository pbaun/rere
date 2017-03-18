package rere.ql.data

case class DatabaseDroppingResult(
  dbsDropped: Long,
  tablesDropped: Long,
  configChanges: Seq[ChangefeedNotification[DatabaseConfig]]
)
