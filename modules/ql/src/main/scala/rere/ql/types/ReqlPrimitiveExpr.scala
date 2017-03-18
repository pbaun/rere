package rere.ql.types

import rere.ql.options.Options
import rere.ql.rasterization.{Renderer, recursive, trampolined}

trait ReqlPrimitiveExpr extends ReqlExpr {
  def command = -1
  def arguments = Nil
  def options = Options.empty
  def repr: String

  override def getRasterizer(renderer: Renderer): recursive.Rasterizer = {
    new recursive.PrimitiveRasterizer(renderer, repr)
  }

  override def getTrampolinedRasterizer(renderer: Renderer): trampolined.Rasterizer = {
    new trampolined.PrimitiveRasterizer(renderer, repr)
  }
}


