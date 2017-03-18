package rere.ql.options

import rere.ql.queries.values

trait IdentifierFormatOptions {

  sealed trait IdentifierFormatOptions extends ComposableOptions

  case object DefaultIdentifierFormat extends IdentifierFormatOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object NameIdentifier extends IdentifierFormatOptions {
    def isEmpty = false
    def view = "identifier_format" -> values.expr("name") :: Nil
    val innerQuery = query
  }

  case object UuidIdentifier extends IdentifierFormatOptions {
    def isEmpty = false
    def view = "identifier_format" -> values.expr("uuid") :: Nil
    val innerQuery = query
  }

}
