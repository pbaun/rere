package rere.ql.types

trait PrimaryKey {
  type Reql <: ReqlDatum
  type Scala
}

object PrimaryKey {
  type Int = PrimaryKey {
    type Reql = ReqlInteger
    type Scala = scala.Int
  }

  type Long = PrimaryKey {
    type Reql = ReqlInteger
    type Scala = scala.Long
  }

  type String = PrimaryKey {
    type Reql = ReqlString
    type Scala = java.lang.String
  }

  type UUID = PrimaryKey {
    type Reql = ReqlUUID
    type Scala = java.util.UUID
  }

  type Time = PrimaryKey {
    type Reql = ReqlTime
    type Scala = java.time.ZonedDateTime
  }
}