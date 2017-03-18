package rere.ql.options

import rere.ql.queries.values

trait ProfileOptions {

  sealed trait ProfileOptions extends ComposableOptions

  case object DefaultProfile extends ProfileOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object DoProfile extends ProfileOptions {
    def isEmpty = false
    def view = "profile" -> values.expr(true) :: Nil
    val innerQuery = query
  }

  case object DontProfile extends ProfileOptions {
    def isEmpty = false
    def view = "profile" -> values.expr(false) :: Nil
    val innerQuery = query
  }

}
