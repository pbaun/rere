package rere.ql.data

import java.util.UUID

import cats.instances.either._
import cats.syntax.apply._
import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject, ObjectEncoder}
import rere.ql.shapes.ModelShape
import rere.ql.shapes.ModelShape.DecodingResult
import rere.ql.types.{PrimaryKey, ReqlObject}
import rere.ql.wire.{ReqlDecoder, ReqlEncoder}

case class TableStatus(
  id: UUID,
  name: String,
  db: String,
  status: TableStatusFlags,
  shards: Seq[TableShardStatus],
  raftLeader: String
)

case class TableStatusFlags(
  readyForOutdatedReads: Boolean,
  readyForReads: Boolean,
  readyForWrites: Boolean,
  allReplicasReady: Boolean
)

case class TableShardStatus(
  primaryReplicas: Seq[String],
  replicas: Seq[TableReplicaStatus]
)

//TODO: enum for state?
case class TableReplicaStatus(
  server: String,
  state: String
)

object TableStatus {
  private val primaryKeyOfTableStatus = Set("id")

  private implicit val tableReplicaStatusDecoder: Decoder[TableReplicaStatus] = Decoder.instance { c =>
    (
      c.downField("server").as[String],
      c.downField("state").as[String]
    ).mapN(TableReplicaStatus.apply)
  }

  private implicit val tableReplicaStatusEncoder: ObjectEncoder[TableReplicaStatus] = ObjectEncoder.instance {
    case TableReplicaStatus(server, state) => JsonObject.fromMap(Map(
      "server" -> server.asJson,
      "state" -> state.asJson
    ))
  }

  private implicit val tableShardStatusDecoder: Decoder[TableShardStatus] = Decoder.instance { c =>
    (
      c.downField("primary_replicas").as[List[String]],
      c.downField("replicas").as[List[TableReplicaStatus]]
    ).mapN(TableShardStatus.apply)
  }

  private implicit val tableShardStatusEncoder: ObjectEncoder[TableShardStatus] = ObjectEncoder.instance {
    case TableShardStatus(primaryReplicas, replicas) => JsonObject.fromMap(Map(
      "primary_replicas" -> primaryReplicas.asJson,
      "replicas" -> replicas.asJson
    ))
  }

  private implicit val tableStatusFlagsDecoder: Decoder[TableStatusFlags] = Decoder.instance { c =>
    (
      c.downField("ready_for_outdated_reads").as[Boolean],
      c.downField("ready_for_reads").as[Boolean],
      c.downField("ready_for_writes").as[Boolean],
      c.downField("all_replicas_ready").as[Boolean]
    ).mapN(TableStatusFlags.apply)
  }

  private implicit val tableStatusFlagsEncoder: ObjectEncoder[TableStatusFlags] = ObjectEncoder.instance {
    case TableStatusFlags(readyForOutdatedReads, readyForReads, readyForWrites, allReplicasReady) =>
      JsonObject.fromMap(Map(
        "ready_for_outdated_reads" -> readyForOutdatedReads.asJson,
        "ready_for_reads" -> readyForReads.asJson,
        "ready_for_writes" -> readyForWrites.asJson,
        "all_replicas_ready" -> allReplicasReady.asJson
      ))
  }

  private implicit val tableStatusDecoder: Decoder[TableStatus] = Decoder.instance { c =>
    (
      c.downField("id").as[UUID],
      c.downField("name").as[String],
      c.downField("db").as[String],
      c.downField("status").as[TableStatusFlags],
      c.downField("shards").as[List[TableShardStatus]],
      c.downField("raft_leader").as[String]
    ).mapN(TableStatus.apply)
  }

  private implicit val tableStatusEncoder: ObjectEncoder[TableStatus] = ObjectEncoder.instance {
    case TableStatus(id, name, db, status, shards, raftLeader) => JsonObject.fromMap(Map(
      "id" -> id.asJson,
      "name" -> name.asJson,
      "db" -> db.asJson,
      "status" -> status.asJson,
      "shards" -> shards.asJson,
      "raft_leader" -> raftLeader.asJson
    ))
  }

  implicit val tableStatusShape = new ModelShape[TableStatus, PrimaryKey.UUID] {
    override def toReqlObject(model: TableStatus): ReqlObject = {
      ReqlEncoder.reqlObjectFromCirce(tableStatusEncoder).encode(model)
    }

    override def toReqlUnidentifiableObject(model: TableStatus): ReqlObject = {
      ReqlEncoder.reqlObjectWithoutPrimaryKeyFromCirce(
        tableStatusEncoder,
        primaryKeyOfTableStatus
      ).encode(model)
    }

    override def fromJson(json: Json): DecodingResult[TableStatus] = {
      val result = ReqlDecoder.reqlFromCirce(tableStatusDecoder).decode(json)
      ModelShape.decodingResultConverter(result, json)
    }
  }

}