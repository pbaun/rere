package rere.ql.options

import rere.ql.types.ReqlInteger

trait HttpRedirectsOptions {

  sealed trait HttpRedirectsOptions extends ComposableOptions

  case object WithDefaultHttpRedirects extends HttpRedirectsOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class WithHttpRedirects(redirects: ReqlInteger) extends HttpRedirectsOptions {
    def isEmpty = false
    def view = "redirects" -> redirects :: Nil
    def expr = exprFromView
  }

}
