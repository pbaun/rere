package rere.ql.types

import rere.ql.queries.values
import rere.ql.typeclasses.Transmuter
import rere.ql.util.{FunctionProxyQuery, ProxyQuery}

// Made specially for .eqJoin, .group [ReqlString | ReqlDatum => ReqlDatum]
trait ReqlDatumSelector[T, +U] extends ReqlExpr

//TODO: move to typeclasses package
object ReqlDatumSelector {

  implicit def stringToDatumSelector[T <: ReqlObject, U <: ReqlDatum](value: String): ReqlDatumSelector[T, U] =
    new ProxyQuery(values.expr(value)) with ReqlDatumSelector[T, U]

  implicit def reqlStringToDatumSelector[T <: ReqlObject, U <: ReqlDatum](value: ReqlString): ReqlDatumSelector[T, U] =
    new ProxyQuery(value) with ReqlDatumSelector[T, U]

  implicit def functionToDatumSelector[T <: ReqlObject : Transmuter, U <: ReqlDatum](f: T => U): ReqlDatumSelector[T, U] =
    new FunctionProxyQuery[T, U](f) with ReqlDatumSelector[T, U]

}
