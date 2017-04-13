package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlString

trait HttpMethodOptions {

  sealed trait HttpMethodOptions extends ComposableOptions

  case object WithDefaultHttpMethod extends HttpMethodOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object HttpGet extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("GET") :: Nil
    val expr = exprFromView
  }

  case object HttpPost extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("POST") :: Nil
    val expr = exprFromView
  }

  case object HttpPut extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("PUT") :: Nil
    val expr = exprFromView
  }

  case object HttpPatch extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("PATCH") :: Nil
    val expr = exprFromView
  }

  case object HttpDelete extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("DELETE") :: Nil
    val expr = exprFromView
  }

  case object HttpHead extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> values.expr("HEAD") :: Nil
    val expr = exprFromView
  }

  case class WithHttpMethod(method: ReqlString) extends HttpMethodOptions {
    def isEmpty = false
    def view = "method" -> method :: Nil
    def expr = exprFromView
  }

}
