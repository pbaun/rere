package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlString

trait HttpResultFormatOptions {

  sealed trait HttpResultFormatOptions extends ComposableOptions

  case object WithDefaultHttpResultFormat extends HttpResultFormatOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object TextResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("text") :: Nil
    def innerQuery = query
  }

  case object JsonResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("json") :: Nil
    def innerQuery = query
  }

  case object JsonpResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("jsonp") :: Nil
    def innerQuery = query
  }

  case object BinaryResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("binary") :: Nil
    def innerQuery = query
  }

  case object AutoResultFormat extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> values.expr("auto") :: Nil
    def innerQuery = query
  }

  case class WithHttpResultFormat(resultFormat: ReqlString) extends HttpResultFormatOptions {
    def isEmpty = false
    def view = "result_format" -> resultFormat :: Nil
    def innerQuery = query
  }

}
