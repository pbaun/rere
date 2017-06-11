package rere.ql.options

trait HttpDataOptions { _: Classes =>

  sealed trait HttpDataOptions extends ComposableOptions

  case object WithoutHttpData extends HttpDataOptions with DefaultOption

  case class WithHttpData(data: HttpData) extends HttpDataOptions with NonDefaultOption {
    def view = "data" -> data.toExpr :: Nil
  }

}
