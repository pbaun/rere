package rere.driver.connection

import java.net.InetSocketAddress

import akka.NotUsed
import akka.actor.ActorSystem
import akka.io.Tcp.SO.KeepAlive
import akka.stream.TLSProtocol._
import akka.stream.scaladsl.Tcp.OutgoingConnection
import akka.stream.scaladsl.{BidiFlow, Flow, Keep, TLS, TLSPlacebo, Tcp}
import akka.stream.{Client, EagerClose}
import akka.util.ByteString
import rere.driver.ConnectionSettings

import scala.concurrent.Future

object TCPTransport {

  private val wrapTls = Flow[ByteString].map(SendBytes)
  private val unwrapTls = Flow[SslTlsInbound].collect { case SessionBytes(_, bytes) => bytes }
  private val tlsBidi = BidiFlow.fromFlows(wrapTls, unwrapTls)

  private def getTLSStage(
    settings: ConnectionSettings
  ): BidiFlow[SslTlsOutbound, ByteString, ByteString, SslTlsInbound, NotUsed] = {
    settings.sslContextProvider match {
      case None => TLSPlacebo()
      case Some(provider) =>
        val sslContext = provider.getSSLContext
        TLS(sslContext, NegotiateNewSession, Client, EagerClose, Some((settings.host, settings.port)))
    }
  }

  def makeConnection(
    settings: ConnectionSettings)(
    implicit actorSystem: ActorSystem
  ): Flow[ByteString, ByteString, Future[OutgoingConnection]] = {
    val transportFlow = Tcp().outgoingConnection(
      InetSocketAddress.createUnresolved(settings.host, settings.port),
      options = KeepAlive(true) :: Nil,
      halfClose = false,
      connectTimeout = settings.connectTimeout)
    val tlsStage = getTLSStage(settings)

    val tlsFlow = tlsStage.joinMat(transportFlow)(Keep.right)
    tlsBidi.joinMat(tlsFlow)(Keep.right)
  }
}
