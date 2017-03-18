package rere.ql.options

import rere.ql.queries.{DSLKeyValuePair, DSLKeyValuePairList, Func, values}
import rere.ql.types._
import rere.ql.values.DSLList

trait Classes {

  case class ServerTag(tag: String)


  sealed abstract class HttpQueryParam(val key: ReqlString, val value: ReqlValue)
  final class HttpQueryNumberParam(key: ReqlString, value: ReqlNumber) extends HttpQueryParam(key, value)
  final class HttpQueryStringParam(key: ReqlString, value: ReqlString) extends HttpQueryParam(key, value)


  sealed trait HttpHeaderFields {
    def toExpr: ReqlExpr
  }

  //Array of full headers like "From: webmaster@w3.org"
  final class HttpHeaderStringField(val field: ReqlString)
  final class HttpHeaderFieldsArray private (fields: ReqlArray[ReqlString]) extends HttpHeaderFields {
    def toExpr: ReqlArray[ReqlString] = fields
  }
  object HttpHeaderFieldsArray {
    def apply(fields: Seq[HttpHeaderStringField]): HttpHeaderFieldsArray = {
      new HttpHeaderFieldsArray(
        new DSLList[ReqlString] {
          override def arguments: List[ReqlExpr] = fields.map(_.field).toList
        }
      )
    }
    def apply(fields: ReqlArray[ReqlString]): HttpHeaderFieldsArray = {
      new HttpHeaderFieldsArray(fields)
    }
  }

  //Map of header fields
  final class HttpHeaderPairField(val fieldName: ReqlString, val fieldValue: ReqlString)
  final class HttpHeaderFieldsObject private (fields: ReqlObject) extends HttpHeaderFields {
    def toExpr: ReqlObject = fields
  }
  object HttpHeaderFieldsObject {
    def apply(fields: Seq[HttpHeaderPairField]): HttpHeaderFieldsObject = {
      new HttpHeaderFieldsObject(
        new DSLKeyValuePairList(fields.foldLeft(Nil: List[DSLKeyValuePair]) {
          (acc, el) => new DSLKeyValuePair(el.fieldName, el.fieldValue) :: acc
        })
      )
    }
    def apply(fields: ReqlObject): HttpHeaderFieldsObject = {
      new HttpHeaderFieldsObject(fields)
    }
  }


  //Expected `data` to be a STRING or OBJECT, but found PTYPE<BINARY> in:
  sealed trait HttpData {
    def toExpr: ReqlExpr
  }

  //string data
  final class HttpStringData(data: ReqlString) extends HttpData {
    def toExpr: ReqlString = data
  }

  //object data
  //Expected `data.abc` to be a NUMBER, STRING or NULL, but found OBJECT in:
  //object[String, Number|String]
  sealed trait HttpFormField {
    def fieldName: ReqlString
    def fieldValue: ReqlValue
  }
  final class HttpNumberFormField(val fieldName: ReqlString, val fieldValue: ReqlNumber) extends HttpFormField
  final class HttpStringFormField(val fieldName: ReqlString, val fieldValue: ReqlString) extends HttpFormField

  final class HttpFormFieldsObject private (fields: ReqlObject) extends HttpData {
    def toExpr: ReqlObject = fields
  }
  object HttpFormFieldsObject {
    def apply(fields: Seq[HttpFormField]): HttpFormFieldsObject = {
      new HttpFormFieldsObject(
        new DSLKeyValuePairList(fields.foldLeft(Nil: List[DSLKeyValuePair]) {
          (acc, el) => new DSLKeyValuePair(el.fieldName, el.fieldValue) :: acc
        })
      )
    }
    def apply(fields: ReqlObject): HttpFormFieldsObject = {
      new HttpFormFieldsObject(fields)
    }
  }

  sealed trait PaginationNextPageStrategy {
    def toExpr: ReqlExpr
  }
  case object LinkNextStrategy extends PaginationNextPageStrategy {
    def toExpr: ReqlString = values.expr("link-next")
  }
  //TODO: function takes special kind of object
  case class PaginationStrategy(urlProvider: ReqlObject => ReqlString) extends PaginationNextPageStrategy {
    def toExpr: ReqlExpr = Func.wrap1(urlProvider)
  }

  sealed trait PaginationPageLimit {
    def toExpr: ReqlInteger
  }
  case object NoLimit extends PaginationPageLimit {
    def toExpr: ReqlInteger = values.expr(-1)
  }
  case object NoRequests extends PaginationPageLimit {
    def toExpr: ReqlInteger = values.expr(0)
  }
  case class RequestsLimit(n: ReqlInteger) extends PaginationPageLimit {
    def toExpr: ReqlInteger = n
  }

}
