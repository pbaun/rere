package rere.ql.util

import rere.ql.queries.Func
import rere.ql.rasterization.{Renderer, recursive, trampolined}
import rere.ql.typeclasses.Transmuter
import rere.ql.types.ReqlExpr

abstract class ProxyQuery(query: ReqlExpr) extends ReqlExpr {
  def command = query.command
  def string = query.string
  def arguments = query.arguments
  def options = query.options

  override def getRasterizer(renderer: Renderer): recursive.Rasterizer = {
    query.getRasterizer(renderer)
  }
  override def getTrampolinedRasterizer(renderer: Renderer): trampolined.Rasterizer = {
    query.getTrampolinedRasterizer(renderer)
  }
}

class FunctionProxyQuery[T <: ReqlExpr : Transmuter, U <: ReqlExpr](f: T => U)
  extends ProxyQuery(Func.wrap1(f))
