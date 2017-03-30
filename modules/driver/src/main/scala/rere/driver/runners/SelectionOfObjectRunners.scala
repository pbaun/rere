package rere.driver.runners

import rere.driver.pool.{AcquireConnection, ConnectionPool}
import rere.driver.protocol.Atom
import rere.driver.runners.ready.SingleValueReadyToGo
import rere.driver.workers.AtomQueryWorker
import rere.ql.extractors.AutoInference
import rere.ql.options.Options
import rere.ql.shapes.ReqlModel
import rere.ql.types.ReqlSelectionOfObject

import scala.concurrent.{Future, Promise}

trait SelectionOfObjectRunners {

  implicit class RunOnReqlSelectionOfObject[InnerExpr, InnerPK](val expr: ReqlSelectionOfObject[InnerExpr, InnerPK]) {
    def run[ScalaType](
      pool: ConnectionPool)(
      implicit inference: AutoInference.Aux[ReqlModel[InnerExpr, InnerPK], ScalaType]
    ): SingleValueReadyToGo[ScalaType] = {
      val runOptions = Options.EmptyOptions
      new ReadyToGoSelectionOfObjectInferred(pool, expr, runOptions, inference)
    }
  }

  class ReadyToGoSelectionOfObjectInferred[InnerExpr, InnerPK, Out](
    pool: ConnectionPool,
    expr: ReqlSelectionOfObject[InnerExpr, InnerPK],
    runOptions: Options,
    inference: AutoInference.Aux[ReqlModel[InnerExpr, InnerPK], Out]
  ) extends SingleValueReadyToGo[Out] {
    override def future(): Future[Out] = {
      val resultPromise = Promise[Out]

      pool.send(
        AcquireConnection(
          Atom,
          context => AtomQueryWorker.props(
            expr,
            runOptions,
            inference.getDecoder,
            resultPromise,
            context.connectionRef,
            context.logger
          ),
          error => {
            resultPromise.tryFailure(error)
            ()
          }
        )
      )

      resultPromise.future
    }
  }

}
