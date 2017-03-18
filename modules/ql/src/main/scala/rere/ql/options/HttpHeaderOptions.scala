package rere.ql.options

trait HttpHeaderOptions { _: Classes =>

  sealed trait HttpHeaderOptions extends ComposableOptions

  case object WithDefaultHttpHeader extends HttpHeaderOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class WithHttpHeader(fields: HttpHeaderFields) extends HttpHeaderOptions {
    def isEmpty = false
    def view = "header" -> fields.toExpr :: Nil
    def innerQuery = query
  }

}
