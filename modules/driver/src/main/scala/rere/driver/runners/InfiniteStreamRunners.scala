package rere.driver.runners

import rere.driver.pool.ConnectionPool
import rere.driver.runners.ready.InfiniteStreamReadyToGo
import rere.ql.extractors.AutoInference
import rere.ql.options.Options
import rere.ql.types.{ReqlDatum, ReqlExpr, ReqlInfiniteStreamLike}

trait InfiniteStreamRunners {
  implicit class RunOnReqlInfiniteStream[InnerExpr <: ReqlDatum](val expr: ReqlInfiniteStreamLike[InnerExpr]) {
    def run[ScalaType, Mat](
      pool: ConnectionPool)(
      implicit inference: AutoInference.Aux[InnerExpr, ScalaType]
    ): InfiniteStreamReadyToGo[ScalaType, Mat] = {
      val runOptions = Options.EmptyOptions
      new ReadyToGoInfiniteStreamInferred(pool, expr, runOptions, inference)
    }
  }

  class ReadyToGoInfiniteStreamInferred[Expr <: ReqlExpr, InnerExpr <: ReqlExpr, Out, Mat](
    pool: ConnectionPool,
    expr: Expr,
    runOptions: Options,
    inference: AutoInference.Aux[InnerExpr, Out]
  ) extends ReadyToGoStreamInferred[Expr, InnerExpr, Out, Mat](pool, expr, runOptions, inference)
    with InfiniteStreamReadyToGo[Out, Mat]
}
