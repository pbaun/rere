package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlString

trait HttpResultFormatOptions {

  sealed trait HttpResultFormatOptions extends ComposableOptions

  case object WithDefaultHttpResultFormat extends HttpResultFormatOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object TextResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("text") :: Nil
    val expr = exprFromView
  }

  case object JsonResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("json") :: Nil
    val expr = exprFromView
  }

  case object JsonpResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("jsonp") :: Nil
    val expr = exprFromView
  }

  case object BinaryResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("binary") :: Nil
    val expr = exprFromView
  }

  case object AutoResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("auto") :: Nil
    val expr = exprFromView
  }

  case class WithHttpResultFormat(resultFormat: ReqlString) extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> resultFormat :: Nil
    def expr = exprFromView
  }

}
