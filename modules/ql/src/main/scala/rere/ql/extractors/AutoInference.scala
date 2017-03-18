package rere.ql.extractors

import java.time.ZonedDateTime

import io.circe.JsonObject
import rere.ql.data._
import rere.ql.shapes.{ModelShape, ReqlModel}
import rere.ql.types._
import rere.ql.wire.ReqlDecoder

trait AutoInference[-Expr <: ReqlExpr] {
  type ScalaType

  def getDecoder: ReqlDecoder[ScalaType]
}

object AutoInference extends LowPriorityAutoInference {

  type Aux[-Expr <: ReqlExpr, Scala] = AutoInference[Expr] {
    type ScalaType = Scala
  }

  def fromDecoder[Reql <: ReqlExpr, Scala](decoder: ReqlDecoder[Scala]): Aux[Reql, Scala] = {
    new AutoInference[Reql] {
      override type ScalaType = Scala
      override def getDecoder: ReqlDecoder[ScalaType] = decoder
    }
  }

  implicit val reqlIntegerAutoInference: Aux[ReqlInteger, Long] =
    fromDecoder(ReqlDecoder.longReqlDecoder)

  implicit val reqlStringAutoInference: Aux[ReqlString, String] =
    fromDecoder(ReqlDecoder.stringReqlDecoder)

  implicit val reqlTimeAutoInference: Aux[ReqlTime, ZonedDateTime] =
    fromDecoder(ReqlDecoder.zonedDataTimeReqlDecoder)

  implicit val reqlJsonObjectAutoInference: Aux[ReqlJsonObject, JsonObject] =
    fromDecoder(ReqlDecoder.jsonObjectReqlDecoder)

  implicit def reqlModelAutoInference[
    M : ModelShape
  ]: Aux[ReqlModel[M], M] =
    fromDecoder(ReqlDecoder.modelReqlDecoder[M])

  implicit def reqlArrayAutoInference[T <: ReqlDatum, Scala](
    implicit inner: Aux[T, Scala]
  ): Aux[ReqlArray[T], Seq[Scala]] =
    fromDecoder(ReqlDecoder.seqReqlDecoder[Scala](inner.getDecoder))

  implicit def reqlChangefeedNotificationOnModelAutoInference[
    ModelType
  ](
    implicit optionTReqlDecoder: ReqlDecoder[Option[ModelType]]
  ): Aux[ReqlChangefeedNotification[ReqlModel[ModelType]], ChangefeedNotification[ModelType]] =
    fromDecoder(ReqlDecoder.changefeedNotificationReqlDecoder(optionTReqlDecoder))

  implicit def reqlModificationResultOnModelAutoInference[
    ModelType : ReqlDecoder
  ]: Aux[ReqlModificationResult[ReqlModel[ModelType]], ModificationResult[ModelType]] =
    fromDecoder(ReqlDecoder.insertionResultReqlDecoder[ModelType])

  implicit def reqlModificationResultOnJsonObjectAutoInference: Aux[ReqlModificationResult[ReqlJsonObject], ModificationResult[JsonObject]] =
    fromDecoder(ReqlDecoder.insertionResultReqlDecoder[JsonObject])

  implicit val reqlDatabaseCreationResultAutoInference: Aux[ReqlDatabaseCreationResult, DatabaseCreationResult] =
    fromDecoder(ReqlDecoder.databaseCreationResultReqlDecoder)

  implicit val reqlDatabaseDroppingResultAutoInference: Aux[ReqlDatabaseDroppingResult, DatabaseDroppingResult] =
    fromDecoder(ReqlDecoder.databaseDroppingResultReqlDecoder)

  implicit val reqlDatabaseConfigResultAutoInference: Aux[ReqlDatabaseConfigResult, DatabaseConfig] =
    fromDecoder(ReqlDecoder.databaseConfigReqlDecoder)

  implicit val reqlTableCreationResultAutoInference: Aux[ReqlTableCreationResult, TableCreationResult] =
    fromDecoder(ReqlDecoder.tableCreationResultReqlDecoder)

  implicit val reqlTableDroppingResultAutoInference: Aux[ReqlTableDroppingResult, TableDroppingResult] =
    fromDecoder(ReqlDecoder.tableDroppingResultReqlDecoder)

  implicit val reqlIndexCreationResultAutoInference: Aux[ReqlIndexCreationResult, IndexCreationResult] =
    fromDecoder(ReqlDecoder.indexCreationResultReqlDecoder)

  implicit val reqlIndexDroppingResultAutoInference: Aux[ReqlIndexDroppingResult, IndexDroppingResult] =
    fromDecoder(ReqlDecoder.indexDroppingResultReqlDecoder)

  implicit val reqlIndexRenamingResultAutoInference: Aux[ReqlIndexRenamingResult, IndexRenamingResult] =
    fromDecoder(ReqlDecoder.indexRenamingResultReqlDecoder)

  implicit val reqlIndexStatusResultAutoInference: Aux[ReqlIndexStatusResult, IndexStatus] =
    fromDecoder(ReqlDecoder.indexStatusReqlDecoder)

  implicit val reqlTableStatusResultAutoInference: Aux[ReqlTableStatusResult, TableStatus] =
    fromDecoder(ReqlDecoder.tableStatusReqlDecoder)

  implicit val reqlTableConfigResultAutoInference: Aux[ReqlTableConfigResult, TableConfig] =
    fromDecoder(ReqlDecoder.tableConfigReqlDecoder)

  implicit val reqlRebalancingResultAutoInference: Aux[ReqlRebalancingResult, RebalancingResult] =
    fromDecoder(ReqlDecoder.rebalancingResultReqlDecoder)

  implicit val reqlReconfiguringResultAutoInference: Aux[ReqlReconfiguringResult, ReconfiguringResult] =
    fromDecoder(ReqlDecoder.reconfiguringResultReqlDecoder)

  implicit val reqlReconfiguringDryResultAutoInference: Aux[ReqlReconfiguringDryResult, ReconfiguringDryResult] =
    fromDecoder(ReqlDecoder.reconfiguringDryResultReqlDecoder)

  implicit val reqlWaitingResultAutoInference: Aux[ReqlWaitingResult, WaitingResult] = {
    fromDecoder(ReqlDecoder.waitingResultReqlDecoder)
  }

  implicit val reqlGrantingResultAutoInference: Aux[ReqlGrantingResult, GrantingResult] = {
    fromDecoder(ReqlDecoder.grantingResultReqlDecoder)
  }

  implicit val reqlSyncingResultAutoInference: Aux[ReqlSyncingResult, SyncingResult] = {
    fromDecoder(ReqlDecoder.syncingResultReqlDecoder)
  }

  implicit val reqlPointAutoInference: Aux[ReqlPoint, GeoPoint] = {
    fromDecoder(ReqlDecoder.geoPointReqlDecoder)
  }

  implicit val reqlLineAutoInference: Aux[ReqlLine, GeoLineString] = {
    fromDecoder(ReqlDecoder.geoLineStringReqlDecoder)
  }

  implicit val reqlPolygonAutoInference: Aux[ReqlPolygon, GeoPolygon] = {
    fromDecoder(ReqlDecoder.geoPolygonReqlDecoder)
  }
}

trait LowPriorityAutoInference extends LowerPriorityAutoInference {
  import AutoInference.{Aux, fromDecoder}

  implicit def reqlChangefeedNotificationAutoInference[
    T <: ReqlDatum
  ](
    implicit optionTReqlDecoder: ReqlDecoder[Option[T]]
  ): Aux[ReqlChangefeedNotification[T], ChangefeedNotification[T]] =
    fromDecoder(ReqlDecoder.changefeedNotificationReqlDecoder(optionTReqlDecoder))

  implicit def reqlModificationResultAutoInference[
    T <: ReqlObject : ReqlDecoder
  ]: Aux[ReqlModificationResult[T], ModificationResult[T]] =
    fromDecoder(ReqlDecoder.insertionResultReqlDecoder[T])
}

trait LowerPriorityAutoInference {

  // With this implicit output type of single value selection query will be ambiguous: if output type
  // of expression is not explicitly specified, compiler can choose this (Aux[ReqlObject, ReqlObjectData, JsonObject])
  // or model (Aux[ReqlModel[M], ReqlObjectData, M]) instance.
  //MAYBE: JsonObject as output type is not best choice: it will contains raw info about pseudo types
  /*implicit val reqlObjectAutoInference: Aux[ReqlObject, ReqlObjectData, JsonObject] = {
    new AutoInference[ReqlObject] {
      override type DataType = ReqlObjectData
      override type ScalaType = JsonObject

      override def getDecoder: ReqlDecoder[JsonObject] = {
        ReqlDecoder.jsonObjectReqlDecoder
      }
    }
  }*/
}