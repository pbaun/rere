package rere.ql.options

trait HttpPaginationOptions { _: Classes =>

  sealed trait HttpPaginationOptions extends ComposableOptions

  case object WithoutHttpPagination extends HttpPaginationOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class WithHttpPagination(page: PaginationNextPageStrategy, pageLimit: PaginationPageLimit) extends HttpPaginationOptions {
    def isEmpty = false
    def view = "page" -> page.toExpr :: "page_limit" -> pageLimit.toExpr :: Nil
    def innerQuery = query
  }

}
