package rere.ql.values

import java.time.ZonedDateTime

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.queries.values
import rere.ql.types._

class ReqlTimeQuery(zonedDateTime: ZonedDateTime) extends ReqlTime {
  //TODO: implementation without BigDecimal and BigInt
  private def seconds: ReqlFloat = {
    val instant = zonedDateTime.toInstant
    val s = instant.getEpochSecond
    val n = instant.getNano
    values.expr(BigDecimal(BigInt(s * 1E9.toLong + n), 9))
  }

  val command = TermType.EPOCH_TIME
  val string = "epoch_time"
  val arguments = seconds :: Nil
  val options = Options.empty
}
