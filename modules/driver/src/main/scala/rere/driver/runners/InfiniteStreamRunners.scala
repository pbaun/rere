package rere.driver.runners

import rere.driver.pool.ConnectionPool
import rere.driver.runners.ready.InfiniteStreamReadyToGo
import rere.ql.extractors.AutoInference
import rere.ql.options.Options
import rere.ql.types.{ReqlDatum, ReqlExpr, ReqlInfiniteStreamLike}

trait InfiniteStreamRunners {
  implicit class RunOnReqlInfiniteStream[InnerExpr <: ReqlDatum](expr: ReqlInfiniteStreamLike[InnerExpr]) {
    def run[ScalaType, OutMat](
      pool: ConnectionPool)(
      implicit inference: AutoInference.Aux[InnerExpr, ScalaType]
    ): InfiniteStreamReadyToGo[ScalaType, OutMat] = {
      val runOptions = Options.EmptyOptions
      new ReadyToGoInfiniteStreamInferred(pool, expr, runOptions, inference)
    }
  }

  class ReadyToGoInfiniteStreamInferred[Expr <: ReqlExpr, InnerExpr <: ReqlExpr, Out, OutMat](
    pool: ConnectionPool,
    expr: Expr,
    runOptions: Options,
    inference: AutoInference.Aux[InnerExpr, Out]
  ) extends ReadyToGoStreamInferred[Expr, InnerExpr, Out, OutMat](pool, expr, runOptions, inference)
    with InfiniteStreamReadyToGo[Out, OutMat]
}
