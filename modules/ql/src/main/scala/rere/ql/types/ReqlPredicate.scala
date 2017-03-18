package rere.ql.types

import rere.ql.typeclasses.ToPredicate

// ReqlDatum | (ReqlDatum => ReqlBoolean)
// Used in .count, .contains
trait ReqlPredicate[T] extends ReqlExpr

object ReqlPredicate {
  implicit def fromToPredicate[T, U <: ReqlDatum](
    value: T)(
    implicit toPredicate: ToPredicate.Aux[T, U]
  ): ReqlPredicate[U] = {
    toPredicate.toPredicate(value)
  }
}
