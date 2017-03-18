package rere.driver.runners

import akka.Done
import akka.stream.scaladsl.Sink
import rere.driver.pool.{AcquireConnection, ConnectionPool}
import rere.driver.protocol.Stream
import rere.driver.workers.StreamQueryWorker
import rere.ql.extractors.AutoInference
import rere.ql.options.Options
import rere.ql.types.ReqlExpr

import scala.concurrent.{Future, Promise}

abstract class ReadyToGoStreamInferred[Expr <: ReqlExpr, InnerExpr <: ReqlExpr, Out, Mat](
  pool: ConnectionPool,
  expr: Expr,
  runOptions: Options,
  inference: AutoInference.Aux[InnerExpr, Out]
) {

  def drainTo(sink: Sink[Out, Mat]): (Future[Mat], Future[Done]) = {
    val matPromise = Promise[Mat]
    val queryDonePromise = Promise[Done]

    pool.send(
      AcquireConnection(
        Stream,
        context => StreamQueryWorker.props(
          expr,
          runOptions,
          inference.getDecoder,
          sink,
          matPromise,
          queryDonePromise,
          context.materializer,
          context.connectionRef,
          context.logger
        ),
        error => {
          matPromise.tryFailure(error)
          queryDonePromise.tryFailure(error)
          ()
        }
      )
    )

    (matPromise.future, queryDonePromise.future)
  }
}
