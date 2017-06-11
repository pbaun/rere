package rere.ql.options

import rere.ql.queries.values

trait IdentifierFormatOptions {

  sealed trait IdentifierFormatOptions extends ComposableOptions

  case object DefaultIdentifierFormat extends IdentifierFormatOptions with DefaultOption

  case object NameIdentifier extends IdentifierFormatOptions with NonDefaultOption {
    def view = "identifier_format" -> values.expr("name") :: Nil
  }

  case object UuidIdentifier extends IdentifierFormatOptions with NonDefaultOption {
    def view = "identifier_format" -> values.expr("uuid") :: Nil
  }

}
