package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlString

trait HttpMethodOptions {

  sealed trait HttpMethodOptions extends ComposableOptions

  case object WithDefaultHttpMethod extends HttpMethodOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object HttpGet extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("GET") :: Nil
    def innerQuery = query
  }

  case object HttpPost extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("POST") :: Nil
    def innerQuery = query
  }

  case object HttpPut extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("PUT") :: Nil
    def innerQuery = query
  }

  case object HttpPatch extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("PATCH") :: Nil
    def innerQuery = query
  }

  case object HttpDelete extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("DELETE") :: Nil
    def innerQuery = query
  }

  case object HttpHead extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("HEAD") :: Nil
    def innerQuery = query
  }

  case class WithHttpMethod(method: ReqlString) extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> method :: Nil
    def innerQuery = query
  }

}
