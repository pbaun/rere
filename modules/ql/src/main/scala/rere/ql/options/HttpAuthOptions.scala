package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlString

trait HttpAuthOptions {

  sealed trait HttpAuthOptions extends ComposableOptions

  case object WithoutHttpAuth extends HttpAuthOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class DefaultAuth(user: ReqlString, pass: ReqlString) extends HttpAuthOptions {
    def isEmpty = false
    def view = "auth" -> values.expr(Map(
      "user" -> user,
      "pass" -> pass
    )) :: Nil
    def innerQuery = query
  }

  case class BasicAuth(user: ReqlString, pass: ReqlString) extends HttpAuthOptions {
    def isEmpty = false
    def view = "auth" -> values.expr(Map(
      "type" -> values.expr("basic"),
      "user" -> user,
      "pass" -> pass
    )) :: Nil
    def innerQuery = query
  }

  case class DigestAuth(user: ReqlString, pass: ReqlString) extends HttpAuthOptions {
    def isEmpty = false
    def view = "auth" -> values.expr(Map(
      "type" -> values.expr("digest"),
      "user" -> user,
      "pass" -> pass
    )) :: Nil
    def innerQuery = query
  }

  case class WithHttpAuth(authType: ReqlString, user: ReqlString, pass: ReqlString) extends HttpAuthOptions {
    def isEmpty = false
    def view = "auth" -> values.expr(Map(
      "type" -> authType,
      "user" -> user,
      "pass" -> pass
    )) :: Nil
    def innerQuery = query
  }

}
