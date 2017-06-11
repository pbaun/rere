package rere.ql.options

import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.types.{ReqlObject, ReqlPrimitiveExpr}

//TODO: make test for checking keys format (should be snake_case)
trait Options {
  def isEmpty: Boolean
  def expr: ReqlObject

  def recursiveRasterizer: recursive.Rasterizer = expr.recursiveRasterizer
  def trampolinedRasterizer: trampolined.Rasterizer = expr.trampolinedRasterizer
}

object Options {
  object EmptyOptions extends Options {
    override def isEmpty: Boolean = true
    override val expr: ReqlObject = new ReqlObject with ReqlPrimitiveExpr {
      override def string: String = "obj"
      override def repr = "{}"
    }
  }

  val empty: Options = EmptyOptions
}
