package rere.sasl.scram.client.impl

import akka.util.ByteString
import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatest.mockito.MockitoSugar
import rere.sasl.gs2.ChannelBindingFlag._
import rere.sasl.scram.client.SaltedPasswordCache
import rere.sasl.scram.crypto.NoOpErrorReporter
import rere.sasl.scram.crypto.entropy.impl.ConstantEntropySource
import rere.sasl.scram.crypto.sha1.ScramSha1AuthMechanismFactory
import rere.sasl.util.{Base64String, PrintableString}

class ClientFirstStepImplTest extends WordSpec with MockitoSugar {

  private val mechanism = ScramSha1AuthMechanismFactory.getMechanism(new NoOpErrorReporter)

  trait mocks {
    val cache = mock[SaltedPasswordCache]
    when(cache.get(any[String](), any[String](), any[Int]())).thenReturn(None)

    val sha1EntropySource = new ConstantEntropySource(new Base64String(""), new PrintableString("fyko+d2lbbFgONRv9qkxdawL"))
    val sha256EntropySource = new ConstantEntropySource(new Base64String(""), new PrintableString("rOprNGfwEbeRWgbNEkqO"))
  }

  "ClientFirstStepImpl.auth" should {
    "produce first message (rfc 5802)" in new mocks {
      val client = new ClientFirstStepImpl(mechanism, sha1EntropySource, cache)
      val (msgByteString, nextClient) =
        client.auth("user", "pencil", NotSupports, None, Nil)

      msgByteString shouldBe ByteString("n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL")
      nextClient should not be null
    }

    "produce first message (rfc 7677)" in new mocks {
      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val (msgByteString, nextClient) =
        client.auth("user", "pencil", NotSupports, None, Nil)

      msgByteString shouldBe ByteString("n,,n=user,r=rOprNGfwEbeRWgbNEkqO")
      nextClient should not be null
    }

    "normalize and escape username" in new mocks {
      val username = "it=is,username～"

      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val (msgByteString, nextClient) =
        client.auth(username, "pencil", NotSupports, None, Nil)

      msgByteString shouldBe ByteString("n,,n=it=3Dis=2Cusername~,r=rOprNGfwEbeRWgbNEkqO")
      nextClient should not be null
    }

    "normalize and escape authId" in new mocks {
      val authId = "it=is,authId～"

      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val (msgByteString, nextClient) =
        client.auth("user", "pencil", NotSupports, Some(authId), Nil)

      msgByteString shouldBe ByteString("n,a=it=3Dis=2CauthId~,n=user,r=rOprNGfwEbeRWgbNEkqO")
      nextClient should not be null
    }

    "support SupportsButNotUsed channel binding mode" in new mocks {
      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val (msgByteString, nextClient) =
        client.auth("user", "pencil", SupportsButNotUsed, None, Nil)

      msgByteString shouldBe ByteString("y,,n=user,r=rOprNGfwEbeRWgbNEkqO")
      nextClient should not be null
    }

    "support SupportsAndUsed channel binding mode" in new mocks {
      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val (msgByteString, nextClient) =
        client.auth("user", "pencil", SupportsAndUsed("tls-unique"), None, Nil)

      msgByteString shouldBe ByteString("p=tls-unique,,n=user,r=rOprNGfwEbeRWgbNEkqO")
      nextClient should not be null
    }

    "not interact with cache" in new mocks {
      val client = new ClientFirstStepImpl(mechanism, sha256EntropySource, cache)
      val (msgByteString, nextClient) =
        client.auth("user", "pencil", NotSupports, None, Nil)

      verifyZeroInteractions(cache)
    }
  }

}
