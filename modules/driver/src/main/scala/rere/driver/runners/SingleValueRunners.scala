package rere.driver.runners

import rere.driver.pool.{AcquireConnection, ConnectionPool}
import rere.driver.protocol.Atom
import rere.driver.runners.ready.SingleValueReadyToGo
import rere.driver.workers.AtomQueryWorker
import rere.ql.extractors.AutoInference
import rere.ql.options.{ComposableOptions, Options}
import rere.ql.types.ReqlExpr

import scala.concurrent.{Future, Promise}

trait SingleValueRunners {

  import rere.ql.options.all._

  implicit class RunOnReqlExpr[Expr <: ReqlExpr](val expr: Expr) {
    def run[ScalaType](
      pool: ConnectionPool,
      readMode: ReadModeOptions = DefaultReadMode,
      profile: ProfileOptions = DefaultProfile,
      durability: DurabilityOptions = DefaultDurability,
      noreply: NoreplyOptions = DefaultNoreply,                //TODO: handle noreply option or make special method???
      arrayLimit: ArrayLimitOptions = DefaultArrayLimit,
      minBatchRows: MinBatchRowsOptions = DefaultMinBatchRows,
      maxBatchRows: MaxBatchRowsOptions = DefaultMaxBatchRows,
      maxBatchBytes: MaxBatchBytesOptions = DefaultMaxBatchBytes,
      maxBatchSeconds: MaxBatchSecondsOptions = DefaultMaxBatchSeconds,
      firstBatchScaledownFactor: FirstBatchScaledownFactorOptions = DefaultFirstBatchScaledownFactor
    )(implicit
      inference: AutoInference.Aux[Expr, ScalaType]
    ): SingleValueReadyToGo[ScalaType] = {
      val runOptions = ComposableOptions.compose(readMode, profile, durability, noreply, arrayLimit,
        minBatchRows, maxBatchRows, maxBatchBytes, maxBatchSeconds, firstBatchScaledownFactor)
      new ReadyToGoValueInferred(pool, expr, runOptions, inference)
    }
  }

  class ReadyToGoValueInferred[Expr <: ReqlExpr, Out](
    pool: ConnectionPool,
    expr: Expr,
    runOptions: Options,
    inference: AutoInference.Aux[Expr, Out]
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
