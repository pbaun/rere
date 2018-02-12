package rere.ql.data

import cats.instances.either._
import cats.syntax.apply._
import io.circe.syntax._
import io.circe.{Decoder, JsonObject, ObjectEncoder}

case class Shard(
  primaryReplica: String,
  replicas: Seq[String],
  nonvotingReplicas: Seq[String]
)

object Shard {
  implicit val shardDecoder: Decoder[Shard] = Decoder.instance { c =>
    (
      c.downField("primary_replica").as[String],
      c.downField("replicas").as[List[String]],
      c.downField("nonvoting_replicas").as[List[String]]
    ).mapN(Shard.apply)
  }

  implicit val shardEncoder: ObjectEncoder[Shard] = ObjectEncoder.instance {
    case Shard(primaryReplica, replicas, nonvotingReplicas) => JsonObject.fromMap(Map(
      "primary_replica" -> primaryReplica.asJson,
      "replicas" -> replicas.asJson,
      "nonvoting_replicas" -> nonvotingReplicas.asJson
    ))
  }
}
