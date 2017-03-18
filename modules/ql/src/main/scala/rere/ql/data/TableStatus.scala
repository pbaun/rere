package rere.ql.data

import java.util.UUID

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
