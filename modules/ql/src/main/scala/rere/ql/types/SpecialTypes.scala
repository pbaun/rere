package rere.ql.types

trait ReqlChangefeedNotification[T] extends ReqlObject

trait ReqlJoinResult[LeftType, RightType] extends ReqlObject

trait ReqlModificationResult[T, PK <: PrimaryKey] extends ReqlObject

/*class ReqlModificationResultImpl[T <: ReqlObject](
    inserted: Long,
    replaced: Long,
    unchanged: Long,
    errors: Long,
    firstError: String,
    deleted: Long,
    skipped: Long,
    generatedKeys: Seq[String],
    warnings: String,// only one?
    changes: Seq[ReqlChangefeedNotification[T]])
  extends ReqlMakeObjFromMapQuery(Map(
    "inserted" -> values.expr(inserted),
    "replaced" -> values.expr(replaced),
    "unchanged" -> values.expr(unchanged),
    "errors" -> values.expr(errors),
    "first_error" -> values.expr(firstError),
    "deleted" -> values.expr(deleted),
    "skipped" -> values.expr(skipped),
    "generated_keys" -> values.expr(generatedKeys.map(values.expr)),
    "warnings" -> values.expr(warnings),
    "changes" -> values.expr(changes)
  ))
  with ReqlModificationResult[T]*/

trait ReqlDatabaseCreationResult extends ReqlObject

trait ReqlDatabaseDroppingResult extends ReqlObject

trait ReqlTableCreationResult extends ReqlObject

trait ReqlTableDroppingResult extends ReqlObject

trait ReqlIndexCreationResult extends ReqlObject

trait ReqlIndexDroppingResult extends ReqlObject

trait ReqlIndexRenamingResult extends ReqlObject

trait ReqlIndexStatusResult extends ReqlObject

trait ReqlRebalancingResult extends ReqlObject

trait ReqlReconfiguringResult extends ReqlObject

trait ReqlReconfiguringDryResult extends ReqlObject

trait ReqlWaitingResult extends ReqlObject

trait ReqlUserPermissions extends ReqlObject

trait ReqlGrantingResult extends ReqlObject

trait ReqlSyncingResult extends ReqlObject

trait ReqlDistanceResult[T] extends ReqlObject
