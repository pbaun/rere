package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlString

trait HttpAuthOptions {

  sealed trait HttpAuthOptions extends ComposableOptions

  case object WithoutHttpAuth extends HttpAuthOptions with DefaultOption

  case class DefaultAuth(user: ReqlString, pass: ReqlString) extends HttpAuthOptions with NonDefaultOption {
    def view = "auth" -> values.expr(Map(
      "user" -> user,
      "pass" -> pass
    )) :: Nil
  }

  case class BasicAuth(user: ReqlString, pass: ReqlString) extends HttpAuthOptions with NonDefaultOption {
    def view = "auth" -> values.expr(Map(
      "type" -> values.expr("basic"),
      "user" -> user,
      "pass" -> pass
    )) :: Nil
  }

  case class DigestAuth(user: ReqlString, pass: ReqlString) extends HttpAuthOptions with NonDefaultOption {
    def view = "auth" -> values.expr(Map(
      "type" -> values.expr("digest"),
      "user" -> user,
      "pass" -> pass
    )) :: Nil
  }

  case class WithHttpAuth(authType: ReqlString, user: ReqlString, pass: ReqlString) extends HttpAuthOptions with NonDefaultOption {
    def view = "auth" -> values.expr(Map(
      "type" -> authType,
      "user" -> user,
      "pass" -> pass
    )) :: Nil
  }

}
