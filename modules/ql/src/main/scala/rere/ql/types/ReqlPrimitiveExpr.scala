package rere.ql.types

import rere.ql.options.Options
import rere.ql.rasterization.{recursive, trampolined}

trait ReqlPrimitiveExpr extends ReqlExpr {
  def command = -1
  def arguments = Nil
  def options = Options.empty
  def repr: String

  override def recursiveRasterizer: recursive.Rasterizer = {
    new recursive.PrimitiveRasterizer(repr)
  }

  override def trampolinedRasterizer: trampolined.Rasterizer = {
    new trampolined.PrimitiveRasterizer(repr)
  }
}
