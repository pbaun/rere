package rere.ql.data

case class ModificationResult[T, PK](
  inserted: Long,
  replaced: Long,
  unchanged: Long,
  errors: Long,
  firstError: Option[String], //String?
  deleted: Long,
  skipped: Long,
  generatedKeys: Option[Seq[PK]],
  warnings: Option[String], //String?
  changes: Option[Seq[ChangefeedNotification[T]]]
)
