package rere.ql.data

import java.util.UUID

import cats.instances.either._
import cats.syntax.cartesian._
import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject, ObjectEncoder}
import rere.ql.queries.values
import rere.ql.shapes.ModelShape
import rere.ql.shapes.ModelShape.DecodingResult
import rere.ql.types.{ReqlDatum, ReqlObject}
import rere.ql.wire.ReqlDecoder.reqlFromCirce
import rere.ql.wire.{ReqlDecoder, ReqlEncoder}

case class DatabaseConfig(id: UUID, name: String)

object DatabaseConfig {
  private val primaryKeyOfDatabaseConfig = Set("id")

  implicit val databaseConfigDecoder: Decoder[DatabaseConfig] = Decoder.instance { c =>
    (
      c.downField("id").as[UUID] |@|
      c.downField("name").as[String]
    ).map(DatabaseConfig.apply)
  }

  implicit val databaseConfigReqlDecoder: ReqlDecoder[DatabaseConfig] = {
    reqlFromCirce[DatabaseConfig](databaseConfigDecoder)
  }

  implicit val databaseConfigEncoder: ObjectEncoder[DatabaseConfig] = ObjectEncoder.instance {
    case DatabaseConfig(id, name) => JsonObject.fromMap(Map(
      "id" -> id.asJson,
      "name" -> name.asJson
    ))
  }

  implicit val databaseConfigShape = new ModelShape[DatabaseConfig, UUID] {
    override def toReqlObject(model: DatabaseConfig): ReqlObject = {
      ReqlEncoder.reqlObjectFromCirce(databaseConfigEncoder).encode(model)
    }

    override def toReqlUnidentifiableObject(model: DatabaseConfig): ReqlObject = {
      ReqlEncoder.reqlObjectWithoutPrimaryKeyFromCirce(
        databaseConfigEncoder,
        primaryKeyOfDatabaseConfig
      ).encode(model)
    }

    override def fromJson(json: Json): DecodingResult[DatabaseConfig] = {
      val result = ReqlDecoder.reqlFromCirce(databaseConfigDecoder).decode(json)
      ModelShape.decodingResultConverter(result, json)
    }

    override def toReqlPrimaryKey(primaryKey: UUID): ReqlDatum = {
      values.expr(primaryKey.asJson)
    }
  }

}