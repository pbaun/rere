package rere.ql.data

import java.util.UUID

import cats.instances.either._
import cats.syntax.apply._
import io.circe._
import io.circe.syntax._
import rere.ql.shapes.ModelShape
import rere.ql.shapes.ModelShape.DecodingResult
import rere.ql.types.{PrimaryKey, ReqlObject}
import rere.ql.wire.{ReqlDecoder, ReqlEncoder}

//TODO: enums for writeAcks and durability?
case class TableConfig(
  id: UUID,
  name: String,
  db: String,
  primaryKey: String,
  shards: Seq[Shard],
  indexes: Seq[String],
  writeAcks: String,
  durability: String
)

object TableConfig {
  private val primaryKeyOfTableConfig = Set("id")

  implicit val tableConfigDecoder: Decoder[TableConfig] = Decoder.instance { c =>
    (
      c.downField("id").as[UUID],
      c.downField("name").as[String],
      c.downField("db").as[String],
      c.downField("primary_key").as[String],
      c.downField("shards").as[List[Shard]],
      c.downField("indexes").as[List[String]],
      c.downField("write_acks").as[String],
      c.downField("durability").as[String]
    ).mapN(TableConfig.apply)
  }

  implicit val tableConfigEncoder: ObjectEncoder[TableConfig] = ObjectEncoder.instance {
    case TableConfig(id, name, db, primaryKey, shards, indexes, writeAcks, durability) =>
      JsonObject.fromMap(Map(
        "id" -> id.asJson,
        "name" -> name.asJson,
        "db" -> db.asJson,
        "primary_key" -> primaryKey.asJson,
        "shards" -> shards.asJson,
        "indexes" -> indexes.asJson,
        "write_acks" -> writeAcks.asJson,
        "durability" -> durability.asJson
      ))
  }

  implicit val tableConfigShape = new ModelShape[TableConfig, PrimaryKey.UUID] {
    override def toReqlObject(model: TableConfig): ReqlObject = {
      ReqlEncoder.reqlObjectFromCirce(tableConfigEncoder).encode(model)
    }

    override def toReqlUnidentifiableObject(model: TableConfig): ReqlObject = {
      ReqlEncoder.reqlObjectWithoutPrimaryKeyFromCirce(
        tableConfigEncoder,
        primaryKeyOfTableConfig
      ).encode(model)
    }

    override def fromJson(json: Json): DecodingResult[TableConfig] = {
      val result = ReqlDecoder.reqlFromCirce(tableConfigDecoder).decode(json)
      ModelShape.decodingResultConverter(result, json)
    }
  }
}
