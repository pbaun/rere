package rere.ql.typeclasses

import rere.ql.queries.values
import rere.ql.types._
import rere.ql.util.{FunctionProxyQuery, ProxyQuery}

// Made specially for .eqJoin, .group [ReqlString | ReqlDatum => ReqlDatum]
trait DatumSelector[T, +U] extends ReqlExpr

object DatumSelector {

  implicit def stringToDatumSelector[T <: ReqlObject, U <: ReqlDatum](value: String): DatumSelector[T, U] =
    new ProxyQuery(values.expr(value)) with DatumSelector[T, U]

  implicit def reqlStringToDatumSelector[T <: ReqlObject, U <: ReqlDatum](value: ReqlString): DatumSelector[T, U] =
    new ProxyQuery(value) with DatumSelector[T, U]

  implicit def functionToDatumSelector[T <: ReqlObject : Transmuter, U <: ReqlDatum](f: T => U): DatumSelector[T, U] =
    new FunctionProxyQuery[T, U](f) with DatumSelector[T, U]

}
