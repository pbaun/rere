package rere.ql.options

import rere.ql.queries.values

trait NumVerticesOptions {

  sealed trait NumVerticesOptions extends ComposableOptions

  case object DefaultNumVertices extends NumVerticesOptions with DefaultOption

  case class NumVertices(n: Integer) extends NumVerticesOptions with NonDefaultOption {
    def view = "num_vertices" -> values.expr(n) :: Nil
  }

}
