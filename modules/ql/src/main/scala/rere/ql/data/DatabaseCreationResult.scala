package rere.ql.data

case class DatabaseCreationResult(
  dbsCreated: Long,
  configChanges: Seq[ChangefeedNotification[DatabaseConfig]]
)
