package rere.ql.options

import rere.ql.types.ReqlInteger

trait HttpRedirectsOptions {

  sealed trait HttpRedirectsOptions extends ComposableOptions

  case object WithDefaultHttpRedirects extends HttpRedirectsOptions with DefaultOption

  case class WithHttpRedirects(redirects: ReqlInteger) extends HttpRedirectsOptions with NonDefaultOption {
    def view = "redirects" -> redirects :: Nil
  }

}
