package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlString

trait HttpMethodOptions {

  sealed trait HttpMethodOptions extends ComposableOptions

  case object WithDefaultHttpMethod extends HttpMethodOptions with DefaultOption

  case object HttpGet extends HttpMethodOptions with NonDefaultOption {
    def view = "method" -> values.expr("GET") :: Nil
  }

  case object HttpPost extends HttpMethodOptions with NonDefaultOption {
    def view = "method" -> values.expr("POST") :: Nil
  }

  case object HttpPut extends HttpMethodOptions with NonDefaultOption {
    def view = "method" -> values.expr("PUT") :: Nil
  }

  case object HttpPatch extends HttpMethodOptions with NonDefaultOption {
    def view = "method" -> values.expr("PATCH") :: Nil
  }

  case object HttpDelete extends HttpMethodOptions with NonDefaultOption {
    def view = "method" -> values.expr("DELETE") :: Nil
  }

  case object HttpHead extends HttpMethodOptions with NonDefaultOption {
    def view = "method" -> values.expr("HEAD") :: Nil
  }

  case class WithHttpMethod(method: ReqlString) extends HttpMethodOptions with NonDefaultOption {
    def view = "method" -> method :: Nil
  }

}
