package rere.driver

import java.nio.file.{Files, Paths}

import rere.driver.connection.{FromCertificateFileProvider, SSLContextProvider}

import scala.concurrent.duration._

case class ConnectionSettings(
  host: String,
  port: Int,
  sslContextProvider: Option[SSLContextProvider],
  connectTimeout: FiniteDuration = 3.seconds,
  checkInterval: FiniteDuration = 1.second,
  firstCheckDelay: FiniteDuration = 3.seconds)

object ConnectionSettings {

  //TODO: helpers for reading ssl config from files, byte arrays etc.

  def sslConnection(pathToCert: String, protocolVersion: TLSProtocolVersion): Option[SSLContextProvider] = {
    val certBytes = Files.readAllBytes(Paths.get(pathToCert))
    Some(new FromCertificateFileProvider(certBytes, protocolVersion))
  }

  def noSslConnection: Option[SSLContextProvider] = {
    None
  }
}

sealed trait TLSProtocolVersion {
  def canonicalName: String
}

object TLSProtocolVersion {
  case object `TLSv1` extends TLSProtocolVersion {
    override def canonicalName: String = "TLSv1"
  }
  case object `TLSv1.1` extends TLSProtocolVersion {
    override def canonicalName: String = "TLSv1.1"
  }
  case object `TLSv1.2` extends TLSProtocolVersion {
    def canonicalName: String = "TLSv1.2"
  }
}

