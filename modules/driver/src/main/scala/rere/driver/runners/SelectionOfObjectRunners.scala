package rere.driver.runners

import rere.driver.pool.ConnectionPool
import rere.driver.runners.ready.SingleValueReadyToGo
import rere.ql.extractors.AutoInference
import rere.ql.options.Options
import rere.ql.types.{PrimaryKey, ReqlModel, ReqlSelectionOfObject}

import scala.concurrent.Future

trait SelectionOfObjectRunners {
  implicit class RunOnReqlSelectionOfObject[InnerExpr, InnerPK <: PrimaryKey](expr: ReqlSelectionOfObject[InnerExpr, InnerPK]) {
    def run[ScalaType](
      pool: ConnectionPool)(
      implicit inference: AutoInference.Aux[ReqlModel[InnerExpr, InnerPK], ScalaType]
    ): SingleValueReadyToGo[ScalaType] = {
      val runOptions = Options.EmptyOptions
      new ReadyToGoSelectionOfObjectInferred(pool, expr, runOptions, inference)
    }
  }

  class ReadyToGoSelectionOfObjectInferred[InnerExpr, InnerPK <: PrimaryKey, Out](
    pool: ConnectionPool,
    expr: ReqlSelectionOfObject[InnerExpr, InnerPK],
    runOptions: Options,
    inference: AutoInference.Aux[ReqlModel[InnerExpr, InnerPK], Out]
  ) extends SingleValueReadyToGo[Out] {
    override def future(): Future[Out] = {
      pool.runAtom(expr, runOptions, inference.getDecoder)
    }
  }
}
