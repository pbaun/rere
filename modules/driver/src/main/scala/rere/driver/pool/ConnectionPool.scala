package rere.driver.pool

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import rere.driver.pool.impl.StreamPool
import rere.driver.{ConnectionSettings, Credentials}
import rere.ql.options.Options
import rere.ql.types.ReqlExpr
import rere.ql.wire.ReqlDecoder

import scala.concurrent.Future

trait ConnectionPool {
  def runAtom[Expr <: ReqlExpr, Out](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out]
  ): Future[Out]

  def runStream[Expr <: ReqlExpr, Out, OutMat](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out],
    outSink: Sink[Out, OutMat]
  ): OutMat

  def shutdown(): Future[PoolShutdownResult]
}

object ConnectionPool {
  def create(
    credentials: Credentials,
    connectionSettings: ConnectionSettings,
    poolName: String,
    poolSize: Int)(
    implicit system: ActorSystem
  ): ConnectionPool = {
    val defaultPoolSettings = ConnectionPoolSettings(poolName, poolSize)
    new StreamPool(credentials, connectionSettings, defaultPoolSettings, system)
  }
}