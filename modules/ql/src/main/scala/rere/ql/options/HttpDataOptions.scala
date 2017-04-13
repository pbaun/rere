package rere.ql.options

trait HttpDataOptions { _: Classes =>

  sealed trait HttpDataOptions extends ComposableOptions

  case object WithoutHttpData extends HttpDataOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class WithHttpData(data: HttpData) extends HttpDataOptions {
    def isEmpty = false
    def view = "data" -> data.toExpr :: Nil
    def expr = exprFromView
  }

}
