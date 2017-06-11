package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlString

trait HttpResultFormatOptions {

  sealed trait HttpResultFormatOptions extends ComposableOptions

  case object WithDefaultHttpResultFormat extends HttpResultFormatOptions with DefaultOption

  case object TextResultFormat extends HttpResultFormatOptions with NonDefaultOption {
    def view = "result_format" -> values.expr("text") :: Nil
  }

  case object JsonResultFormat extends HttpResultFormatOptions with NonDefaultOption {
    def view = "result_format" -> values.expr("json") :: Nil
  }

  case object JsonpResultFormat extends HttpResultFormatOptions with NonDefaultOption {
    def view = "result_format" -> values.expr("jsonp") :: Nil
  }

  case object BinaryResultFormat extends HttpResultFormatOptions with NonDefaultOption {
    def view = "result_format" -> values.expr("binary") :: Nil
  }

  case object AutoResultFormat extends HttpResultFormatOptions with NonDefaultOption {
    def view = "result_format" -> values.expr("auto") :: Nil
  }

  case class WithHttpResultFormat(resultFormat: ReqlString) extends HttpResultFormatOptions with NonDefaultOption {
    def view = "result_format" -> resultFormat :: Nil
  }

}
