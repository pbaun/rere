package rere.ql.options

trait HttpPaginationOptions { _: Classes =>

  sealed trait HttpPaginationOptions extends ComposableOptions

  case object WithoutHttpPagination extends HttpPaginationOptions with DefaultOption

  case class WithHttpPagination(page: PaginationNextPageStrategy, pageLimit: PaginationPageLimit)
    extends HttpPaginationOptions
    with NonDefaultOption {

    def view = "page" -> page.toExpr :: "page_limit" -> pageLimit.toExpr :: Nil
  }

}
