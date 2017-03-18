package rere.driver.runners

import rere.driver.pool.ConnectionPool
import rere.driver.runners.ready.FiniteStreamReadyToGo
import rere.ql.extractors.AutoInference
import rere.ql.options.Options
import rere.ql.types.{ReqlDatum, ReqlExpr, ReqlFiniteStreamLike}

trait FiniteStreamRunners {

  implicit class RunOnReqlFiniteStream[InnerExpr <: ReqlDatum](val expr: ReqlFiniteStreamLike[InnerExpr]) {
    def run[ScalaType, Mat](
      pool: ConnectionPool)(
      implicit inference: AutoInference.Aux[InnerExpr, ScalaType]
    ): FiniteStreamReadyToGo[ScalaType, Mat] = {
      val runOptions = Options.EmptyOptions
      new ReadyToGoFiniteStreamInferred(pool, expr, runOptions, inference)
    }
  }

  class ReadyToGoFiniteStreamInferred[Expr <: ReqlExpr, InnerExpr <: ReqlExpr, Out, Mat](
    pool: ConnectionPool,
    expr: Expr,
    runOptions: Options,
    inference: AutoInference.Aux[InnerExpr, Out]
  ) extends ReadyToGoStreamInferred[Expr, InnerExpr, Out, Mat](pool, expr, runOptions, inference)
    with FiniteStreamReadyToGo[Out, Mat]
}
