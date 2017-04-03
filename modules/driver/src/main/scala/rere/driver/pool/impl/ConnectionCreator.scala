package rere.driver.pool.impl

import akka.Done
import akka.actor.{Actor, ActorRef, ActorSystem}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Tcp.OutgoingConnection
import rere.driver.auth.AuthCommander
import rere.driver.connection.{Interconnections, LogicalConnection, TCPTransport}
import rere.driver.logger.{Logger, POOL}
import rere.driver.util.StreamsDebugging
import rere.driver.{ConnectionSettings, Credentials}
import rere.sasl.scram.cache.SaltedPasswordCache
import rere.sasl.scram.client.SCRAMClient
import rere.sasl.scram.crypto.ErrorReporter
import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.scram.crypto.sha256.ScramSha256AuthMechanismFactory

import scala.concurrent.Future

trait ConnectionCreator { _: Actor =>
  implicit def actorSystem: ActorSystem
  implicit def materializer: ActorMaterializer

  case class ConnectionDescriptor(
    connectionRef: ActorRef,
    connectionFuture: Future[OutgoingConnection],
    connectionDoneFuture: Future[Done]
  )

  def makeOneConnection(
    connectionId: Long,
    credentials: Credentials,
    connectionSettings: ConnectionSettings,
    entropySource: EntropySource,
    saltedPasswordCache: SaltedPasswordCache,
    logger: Logger
  ): ConnectionDescriptor = {
    val watchedTcpConnection = TCPTransport
      .makeConnection(connectionSettings)
      .joinMat(StreamsDebugging.loggingBidi)(Keep.left)
      .watchTermination()(Keep.both)

    val logicalConnectionFlow = LogicalConnection.makeLogicalConnection(
      parentContext = context,
      name = s"logical_connection-$connectionId",
      logger = logger
    )

    val errorReporter = new ErrorReporter {
      override def onError(throwable: Throwable): Unit = {
        logger.log(POOL, self.toString(), s"Cryptographic error on connection $connectionId: $throwable")
      }
    }
    val mechanism = ScramSha256AuthMechanismFactory.getMechanism(errorReporter)

    val graph = Interconnections.connectToSwitcher(
      connection = watchedTcpConnection,
      commander = AuthCommander.getCommanderFlow(
        new AuthCommander(
          SCRAMClient(mechanism, entropySource, saltedPasswordCache),
          credentials.login,
          credentials.password
        )
      ),
      dataFlow = logicalConnectionFlow
    ).named(s"connection_graph-$connectionId")

    val ((connectionFuture, doneFuture), connectionRef) = graph.run()

    ConnectionDescriptor(connectionRef, connectionFuture, doneFuture)
  }
}