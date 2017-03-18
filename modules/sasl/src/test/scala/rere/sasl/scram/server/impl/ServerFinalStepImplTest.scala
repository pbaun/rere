package rere.sasl.scram.server.impl

import org.scalatest.mockito.MockitoSugar
import org.scalatest.{Matchers, WordSpec}
import rere.sasl.gs2
import rere.sasl.gs2.ChannelBindingFlag
import rere.sasl.gs2.ChannelBindingFlag.NotSupports
import rere.sasl.scram.crypto.NoOpErrorReporter
import rere.sasl.scram.crypto.sha1.ScramSha1AuthMechanismFactory
import rere.sasl.scram.messages.{ServerFinalMessage, _}
import rere.sasl.scram.server.AuthData
import rere.sasl.util._

class ServerFinalStepImplTest extends WordSpec with MockitoSugar with Matchers {

  private val mechanism = ScramSha1AuthMechanismFactory.getMechanism(new NoOpErrorReporter)

  trait mocks {
    val clientFirstMessage = ClientFirstMessage(
      gs2.Header(NotSupports, None),
      ClientFirstMessageBare(
        None,
        EscapedString.to("user"),
        new PrintableString("fyko+d2lbbFgONRv9qkxdawL"),
        Nil
      )
    )

    val serverFirstMessage = ServerFirstMessage(
      None,
      new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
      new Base64String("QSXCR+Q6sek8bf92"),
      4096,
      Nil
    )

    val authData = AuthData(
      username = "user",
      saltedPassword = Base64.from(new Base64String("HZbuOlKbWl+eR8AfIposuKbhX30=")),
      salt = Base64.from(new Base64String("QSXCR+Q6sek8bf92")),
      i = 4096,
      isReal = true,
      clientKey = Base64.from(new Base64String("4jTEe/bDZpbdbYUrmaqiuiZVVyg=")),
      storedKey = Base64.from(new Base64String("6dlGYMOdZcOPutkcNY8U2g7vK9Y="))
    )

    val serverNonce = new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j")

    val clientFinalMessage = ClientFinalMessage(
      ClientFinalMessageWithoutProof(
        new Base64String("biws"),
        new Base64String("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
        Nil
      ),
      new Base64String("v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=")
    )
  }

  "ServerFinalStepImpl" should {
    "behave like described in rfc 5802" in new mocks {
      val server = new ServerFinalStepImpl(
        clientFirstMessage,
        serverFirstMessage,
        authData,
        serverNonce,
        mechanism
      )

      server.process(clientFinalMessage) shouldBe ServerFinalMessage(Right(new Base64String("rmF9pqV8S7suAoZWja4dJRkFsKQ=")), Nil)
    }

    "send InvalidProof error if proof in final client message is not valid" in new mocks {
      val server = new ServerFinalStepImpl(
        clientFirstMessage,
        serverFirstMessage,
        authData,
        serverNonce,
        mechanism
      )

      val clientFinalMessageWithInvalidProof = clientFinalMessage.copy(
        proof = new Base64String("00X8v3Bz2T0CJGbJQyF0X+HI4Ts=")
      )

      server.process(clientFinalMessageWithInvalidProof) shouldBe ServerFinalMessage(Left(ServerError(ServerErrorType.InvalidProof)), Nil)
    }

    "send InvalidProof error if client key in final client message has invalid length" in new mocks {
      val server = new ServerFinalStepImpl(
        clientFirstMessage,
        serverFirstMessage,
        authData.copy(clientKey = Base64.from(new Base64String("4jTEe/bDZpbdbYUrmaqiuiZV"))),
        serverNonce,
        mechanism
      )

      server.process(clientFinalMessage) shouldBe ServerFinalMessage(Left(ServerError(ServerErrorType.InvalidProof)), Nil)
    }

    "send InvalidProof error if proof in final client message has invalid length" in new mocks {
      val server = new ServerFinalStepImpl(
        clientFirstMessage,
        serverFirstMessage,
        authData,
        serverNonce,
        mechanism
      )

      val clientFinalMessageWithTruncatedProof = clientFinalMessage.copy(
        proof = new Base64String("v0X8v3Bz2T0CJGbJQyF0X+HI")
      )

      server.process(clientFinalMessageWithTruncatedProof) shouldBe ServerFinalMessage(Left(ServerError(ServerErrorType.InvalidProof)), Nil)
    }

    "send ServerDoesSupportChannelBinding if client first message gs2 header require channel binding support" in new mocks {
      val clientFirstMessageWithChannelBinding = clientFirstMessage.copy(
        header = clientFirstMessage.header.copy(
          channelBinding = ChannelBindingFlag.SupportsButNotUsed
        )
      )

      val server = new ServerFinalStepImpl(
        clientFirstMessageWithChannelBinding,
        serverFirstMessage,
        authData,
        serverNonce,
        mechanism
      )

      val clientFinalMessageWithChannelBinding = clientFinalMessage.copy(
        bare = clientFinalMessage.bare.copy(
          channelBinding = new Base64String("eSws")
        )
      )

      server.process(clientFinalMessageWithChannelBinding) shouldBe ServerFinalMessage(Left(ServerError(ServerErrorType.ServerDoesSupportChannelBinding)), Nil)
    }

    "send ChannelBindingsDontMatch if channel binding mode in first and final client message is not the same" in new mocks {
      val server = new ServerFinalStepImpl(
        clientFirstMessage,
        serverFirstMessage,
        authData,
        serverNonce,
        mechanism
      )

      val clientFinalMessageWithChannelBinding = clientFinalMessage.copy(
        bare = clientFinalMessage.bare.copy(
          channelBinding = new Base64String("eSws")
        )
      )

      server.process(clientFinalMessageWithChannelBinding) shouldBe ServerFinalMessage(Left(ServerError(ServerErrorType.ChannelBindingsDontMatch)), Nil)
    }

    "send InvalidEncoding if final client message contain invalid nonce" in new mocks {
      val server = new ServerFinalStepImpl(
        clientFirstMessage,
        serverFirstMessage,
        authData,
        serverNonce,
        mechanism
      )

      val clientFinalMessageWithTruncatedNonce = clientFinalMessage.copy(
        bare = clientFinalMessage.bare.copy(
          nonce = new Base64String("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvW")
        )
      )

      server.process(clientFinalMessageWithTruncatedNonce) shouldBe ServerFinalMessage(Left(ServerError(ServerErrorType.InvalidEncoding)), Nil)
    }

    "send UnknownUser if auth data was generated by storage because storage not contains salted password for him" in new mocks {
      val generatedAuthData = authData.copy(isReal = false)

      val server = new ServerFinalStepImpl(
        clientFirstMessage,
        serverFirstMessage,
        generatedAuthData,
        serverNonce,
        mechanism
      )

      server.process(clientFinalMessage) shouldBe ServerFinalMessage(Left(ServerError(ServerErrorType.UnknownUser)), Nil)
    }
  }

}
