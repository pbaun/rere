package rere.ql.options

import rere.ql.queries.values

trait ReadModeOptions {

  sealed trait ReadModeOptions extends ComposableOptions

  case object DefaultReadMode extends ReadModeOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object Single extends ReadModeOptions {
    def isEmpty = false
    def view = "read_mode" -> values.expr("single") :: Nil
    val innerQuery = query
  }

  case object Majority extends ReadModeOptions {
    def isEmpty = false
    def view = "read_mode" -> values.expr("majority") :: Nil
    val innerQuery = query
  }

  case object Outdated extends ReadModeOptions {
    def isEmpty = false
    def view = "read_mode" -> values.expr("outdated") :: Nil
    val innerQuery = query
  }

}
