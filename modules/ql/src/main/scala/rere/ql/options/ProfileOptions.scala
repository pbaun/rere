package rere.ql.options

import rere.ql.queries.values

trait ProfileOptions {

  sealed trait ProfileOptions extends ComposableOptions

  case object DefaultProfile extends ProfileOptions with DefaultOption

  case object DoProfile extends ProfileOptions with NonDefaultOption {
    def view = "profile" -> values.expr(true) :: Nil
  }

  case object DontProfile extends ProfileOptions with NonDefaultOption {
    def view = "profile" -> values.expr(false) :: Nil
  }

}
