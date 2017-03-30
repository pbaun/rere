package rere.ql.util

import rere.ql.shapes.{ModelShape, ReqlModel}
import rere.ql.types._

class NullHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlNull
class BooleanHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlBoolean

//TODO: deprecate number proxy???
class NumberHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlNumber
class IntegerHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlInteger
class FloatHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlFloat

class StringHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlString
class ArrayHintProxy[T <: ReqlDatum](query: ReqlExpr) extends ProxyQuery(query) with ReqlArray[T]
class ObjectHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlObject
class JsonHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlJson
class JsonObjectHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlJsonObject

class BinaryHintProxy(query: ReqlExpr) extends ProxyQuery(query) with ReqlBinary
class ModelHintProxy[T, PK](query: ReqlExpr, val shape: ModelShape[T, PK]) extends ProxyQuery(query) with ReqlModel[T, PK]
