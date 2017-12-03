package rere.ql.values

import java.util.UUID

import rere.ql.types.ReqlUUID

class ReqlUUIDQuery(uuidValue: UUID) extends ReqlStringQuery(uuidValue.toString) with ReqlUUID
