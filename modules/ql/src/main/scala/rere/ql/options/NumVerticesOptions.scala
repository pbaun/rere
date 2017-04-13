package rere.ql.options

import rere.ql.queries.values

trait NumVerticesOptions {

  sealed trait NumVerticesOptions extends ComposableOptions

  case object DefaultNumVertices extends NumVerticesOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class NumVertices(n: Integer) extends NumVerticesOptions {
    def isEmpty = false
    def view = "num_vertices" -> values.expr(n) :: Nil
    def expr = exprFromView
  }

}
