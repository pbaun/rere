package rere.driver.runners

import akka.stream.scaladsl.Sink
import rere.driver.pool.ConnectionPool
import rere.ql.extractors.AutoInference
import rere.ql.options.Options
import rere.ql.types.ReqlExpr

abstract class ReadyToGoStreamInferred[Expr <: ReqlExpr, InnerExpr <: ReqlExpr, Out, OutMat](
    pool: ConnectionPool,
    expr: Expr,
    runOptions: Options,
    inference: AutoInference.Aux[InnerExpr, Out]
  ) {

  def drainTo(sink: Sink[Out, OutMat]): OutMat = {
    pool.runStream(expr, runOptions, inference.getDecoder, sink)
  }
}
