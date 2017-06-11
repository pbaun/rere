package rere.ql.options

import rere.ql.queries.values

trait ReadModeOptions {

  sealed trait ReadModeOptions extends ComposableOptions

  case object DefaultReadMode extends ReadModeOptions with DefaultOption

  case object Single extends ReadModeOptions with NonDefaultOption {
    def view = "read_mode" -> values.expr("single") :: Nil
  }

  case object Majority extends ReadModeOptions with NonDefaultOption {
    def view = "read_mode" -> values.expr("majority") :: Nil
  }

  case object Outdated extends ReadModeOptions with NonDefaultOption {
    def view = "read_mode" -> values.expr("outdated") :: Nil
  }

}
