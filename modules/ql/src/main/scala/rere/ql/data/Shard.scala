package rere.ql.data

case class Shard(
  primaryReplica: String,
  replicas: Seq[String],
  nonvotingReplicas: Seq[String]
)
