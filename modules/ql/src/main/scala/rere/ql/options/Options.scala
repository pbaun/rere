package rere.ql.options

import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.types.{ReqlExpr, ReqlPrimitiveExpr}

//TODO: make test for checking keys format (should be snake_case)
trait Options {
  def isEmpty: Boolean
  def expr: ReqlExpr

  def recursiveRasterizer: recursive.Rasterizer = expr.recursiveRasterizer
  def trampolinedRasterizer: trampolined.Rasterizer = expr.trampolinedRasterizer
}

object Options {
  object EmptyOptions extends Options {
    override def isEmpty = true
    override val expr: ReqlExpr = new ReqlPrimitiveExpr {
      def string: String = "obj"
      def repr = "{}"
    }
  }

  val empty: Options = EmptyOptions
}
