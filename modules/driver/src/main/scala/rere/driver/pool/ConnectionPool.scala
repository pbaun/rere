package rere.driver.pool

import akka.actor.ActorSystem
import rere.driver.logger.impl.VerboseLogger
import rere.driver.pool.impl.{P2CConnectionPool, P2CConnectionPoolActor}
import rere.driver.{ConnectionSettings, Credentials}

import scala.concurrent.Future
import scala.concurrent.duration._

trait ConnectionPool {
  def send(message: ConnectionPoolIncomingMessages): Unit
  def shutdown(): Future[ConnectionPoolShutdownResult]
}

object ConnectionPool {
  def create(
    credentials: Credentials,
    connectionSettings: ConnectionSettings,
    poolName: String,
    poolSize: Int,
    reconnectTimeout: FiniteDuration = 1.second
  )(
    implicit system: ActorSystem
  ): ConnectionPool = {
    val logger = new VerboseLogger()
    val connectionPoolActorRef = system.actorOf(
      P2CConnectionPoolActor.props(
        credentials, connectionSettings, poolSize, reconnectTimeout, logger
      ),
      poolName
    )
    new P2CConnectionPool(connectionPoolActorRef, logger)
  }
}
