package rere.ql.data

import java.util.UUID

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
