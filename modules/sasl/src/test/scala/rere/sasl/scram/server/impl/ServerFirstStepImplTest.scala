package rere.sasl.scram.server.impl

import akka.util.ByteString
import org.mockito.Mockito._
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatest.mockito.MockitoSugar
import rere.sasl._
import rere.sasl.gs2.ChannelBindingFlag.NotSupports
import rere.sasl.scram.crypto.NoOpErrorReporter
import rere.sasl.scram.crypto.entropy.impl.ConstantEntropySource
import rere.sasl.scram.crypto.sha1.ScramSha1AuthMechanismFactory
import rere.sasl.scram.messages.{ClientFirstMessage, ClientFirstMessageBare}
import rere.sasl.scram.server.{AuthData, SaltedPasswordStorage}
import rere.sasl.util.{Base64, Base64String, EscapedString, PrintableString}

class ServerFirstStepImplTest extends WordSpec with MockitoSugar {

  private val mechanism = ScramSha1AuthMechanismFactory.getMechanism(new NoOpErrorReporter)

  "ServerFirstStepImpl" should {
    "behave like described in rfc 5802" in {
      val storage = mock[SaltedPasswordStorage]
      when(storage.getAuthData("user")).thenReturn(
        AuthData(
          username = "user",
          saltedPassword = Base64.from(new Base64String("HZbuOlKbWl+eR8AfIposuKbhX30=")),
          salt = Base64.from(new Base64String("QSXCR+Q6sek8bf92")),
          i = 4096,
          isReal = true,
          clientKey = Base64.from(new Base64String("4jTEe/bDZpbdbYUrmaqiuiZVVyg=")),
          storedKey = Base64.from(new Base64String("6dlGYMOdZcOPutkcNY8U2g7vK9Y="))
        )
      )

      val nonceEntropySource = new ConstantEntropySource(new Base64String(""), new PrintableString("3rfcNHYJY1ZVvWVs7j"))
      val server = new ServerFirstStepImpl(mechanism, nonceEntropySource, storage)

      val clientFirstMessage = ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          EscapedString.to("user"),
          new PrintableString("fyko+d2lbbFgONRv9qkxdawL"),
          Nil
        )
      )

      val (msgByteString, nextServer) = server.process(clientFirstMessage)

      msgByteString shouldBe ByteString("r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096")

      nextServer should not be null

    }
  }

  val sha1SaltedPassword = "HZbuOlKbWl+eR8AfIposuKbhX30="

  val sha256SaltedPassword = "xKSVEDI6tPlSysH6mUQZOeeOp01r6B3fcJbodRPcYV0="

}
