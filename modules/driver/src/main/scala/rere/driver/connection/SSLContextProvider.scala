package rere.driver.connection

import java.io.ByteArrayInputStream
import java.security.KeyStore
import java.security.cert.{CertificateFactory, X509Certificate}
import javax.net.ssl.{SSLContext, TrustManagerFactory}

import rere.driver.TLSProtocolVersion

sealed trait SSLContextProvider {
  def getSSLContext: SSLContext
}

class PreparedSSLContextProvider(context: SSLContext) extends SSLContextProvider {
  override def getSSLContext: SSLContext = context
}

class FromCertificateFileProvider(bytes: Array[Byte], protocolVersion: TLSProtocolVersion) extends SSLContextProvider {
  override def getSSLContext: SSLContext = {
    val certificateFactory = CertificateFactory.getInstance("X.509")
    val certificate = certificateFactory.generateCertificate(new ByteArrayInputStream(bytes)).asInstanceOf[X509Certificate]

    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    val keyStore = KeyStore.getInstance(KeyStore.getDefaultType)
    keyStore.load(null)
    keyStore.setCertificateEntry("rootCa", certificate)
    trustManagerFactory.init(keyStore)

    val context = SSLContext.getInstance(protocolVersion.canonicalName)
    context.init(null, trustManagerFactory.getTrustManagers, null)
    context
  }
}
