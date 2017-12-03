package rere.ql.data

case class ModificationResult[T, ScalaPK](
  inserted: Long,
  replaced: Long,
  unchanged: Long,
  errors: Long,
  firstError: Option[String], //String?
  deleted: Long,
  skipped: Long,
  generatedKeys: Option[Seq[ScalaPK]],
  warnings: Option[String], //String?
  changes: Option[Seq[ChangefeedNotification[T]]]
)
