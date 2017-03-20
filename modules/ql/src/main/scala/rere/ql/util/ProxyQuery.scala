package rere.ql.util

import rere.ql.queries.Func
import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.typeclasses.Transmuter
import rere.ql.types.ReqlExpr

abstract class ProxyQuery(query: ReqlExpr) extends ReqlExpr {
  def command = query.command
  def string = query.string
  def arguments = query.arguments
  def options = query.options

  override def recursiveRasterizer: recursive.Rasterizer = query.recursiveRasterizer
  override def trampolinedRasterizer: trampolined.Rasterizer = query.trampolinedRasterizer
}

class FunctionProxyQuery[T <: ReqlExpr : Transmuter, U <: ReqlExpr](f: T => U)
  extends ProxyQuery(Func.wrap1(f))
