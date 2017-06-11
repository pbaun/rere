package rere.ql.options

trait HttpHeaderOptions { _: Classes =>

  sealed trait HttpHeaderOptions extends ComposableOptions

  case object WithDefaultHttpHeader extends HttpHeaderOptions with DefaultOption

  case class WithHttpHeader(fields: HttpHeaderFields) extends HttpHeaderOptions with NonDefaultOption {
    def view = "header" -> fields.toExpr :: Nil
  }

}
