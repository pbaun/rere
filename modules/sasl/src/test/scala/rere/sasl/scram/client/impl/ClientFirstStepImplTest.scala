package rere.sasl.scram.client.impl

import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import rere.sasl.gs2
import rere.sasl.gs2.ChannelBindingFlag._
import rere.sasl.scram.cache.SaltedPasswordCache
import rere.sasl.scram.crypto.NoOpErrorReporter
import rere.sasl.scram.crypto.entropy.impl.ConstantEntropySource
import rere.sasl.scram.crypto.sha1.ScramSha1AuthMechanismFactory
import rere.sasl.scram.messages.{ClientFirstMessage, ClientFirstMessageBare}
import rere.sasl.util.{Base64String, EscapedString, PrintableString}

class ClientFirstStepImplTest extends WordSpec with Matchers with MockFactory {

  private val mechanism = ScramSha1AuthMechanismFactory.getMechanism(new NoOpErrorReporter)

  trait mocks {
    val cache = mock[SaltedPasswordCache]

    val sha1EntropySource = new ConstantEntropySource(new Base64String(""), new PrintableString("fyko+d2lbbFgONRv9qkxdawL"))
    val sha256EntropySource = new ConstantEntropySource(new Base64String(""), new PrintableString("rOprNGfwEbeRWgbNEkqO"))
  }

  "ClientFirstStepImpl.auth" should {
    "produce first message (rfc 5802)" in new mocks {
      val client = new ClientFirstStepImpl(mechanism, sha1EntropySource, cache)
      val nextClient = client.auth("user", "pencil", NotSupports, None, Nil)

      nextClient.firstMessage shouldBe ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          new EscapedString("user"),
          new PrintableString("fyko+d2lbbFgONRv9qkxdawL"),
          Nil
        )
      )
    }

    "produce first message (rfc 7677)" in new mocks {
      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val nextClient = client.auth("user", "pencil", NotSupports, None, Nil)

      nextClient.firstMessage shouldBe ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          new EscapedString("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )
    }

    "normalize and escape username" in new mocks {
      val username = "it=is,username～"

      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val nextClient = client.auth(username, "pencil", NotSupports, None, Nil)

      nextClient.firstMessage shouldBe ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          new EscapedString("it=3Dis=2Cusername~"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )
    }

    "normalize and escape authId" in new mocks {
      val authId = "it=is,authId～"

      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val nextClient = client.auth("user", "pencil", NotSupports, Some(authId), Nil)

      nextClient.firstMessage shouldBe ClientFirstMessage(
        gs2.Header(NotSupports, Some(new EscapedString("it=3Dis=2CauthId~"))),
        ClientFirstMessageBare(
          None,
          new EscapedString("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )
    }

    "support SupportsButNotUsed channel binding mode" in new mocks {
      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val nextClient = client.auth("user", "pencil", SupportsButNotUsed, None, Nil)

      nextClient.firstMessage shouldBe ClientFirstMessage(
        gs2.Header(SupportsButNotUsed, None),
        ClientFirstMessageBare(
          None,
          new EscapedString("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )
    }

    "support SupportsAndUsed channel binding mode" in new mocks {
      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val nextClient = client.auth("user", "pencil", SupportsAndUsed("tls-unique"), None, Nil)

      nextClient.firstMessage shouldBe ClientFirstMessage(
        gs2.Header(SupportsAndUsed("tls-unique"), None),
        ClientFirstMessageBare(
          None,
          new EscapedString("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )
    }
  }

}
