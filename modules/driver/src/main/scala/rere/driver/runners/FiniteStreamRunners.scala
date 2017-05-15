package rere.driver.runners

import rere.driver.pool.ConnectionPool
import rere.driver.runners.ready.FiniteStreamReadyToGo
import rere.ql.extractors.AutoInference
import rere.ql.options.Options
import rere.ql.types.{ReqlDatum, ReqlExpr, ReqlFiniteStreamLike}

trait FiniteStreamRunners {
  implicit class RunOnReqlFiniteStream[InnerExpr <: ReqlDatum](expr: ReqlFiniteStreamLike[InnerExpr]) {
    def run[ScalaType, OutMat](
      pool: ConnectionPool)(
      implicit inference: AutoInference.Aux[InnerExpr, ScalaType]
    ): FiniteStreamReadyToGo[ScalaType, OutMat] = {
      val runOptions = Options.EmptyOptions
      new ReadyToGoFiniteStreamInferred(pool, expr, runOptions, inference)
    }
  }

  class ReadyToGoFiniteStreamInferred[Expr <: ReqlExpr, InnerExpr <: ReqlExpr, Out, OutMat](
    pool: ConnectionPool,
    expr: Expr,
    runOptions: Options,
    inference: AutoInference.Aux[InnerExpr, Out]
  ) extends ReadyToGoStreamInferred[Expr, InnerExpr, Out, OutMat](pool, expr, runOptions, inference)
    with FiniteStreamReadyToGo[Out, OutMat]
}
