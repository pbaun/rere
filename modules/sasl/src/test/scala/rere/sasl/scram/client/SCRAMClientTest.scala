package rere.sasl.scram.client

import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import rere.sasl.scram.cache.SaltedPasswordCache
import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.crypto.entropy.EntropySource

class SCRAMClientTest extends WordSpec with Matchers with MockFactory {

  "SCRAMClient object" should {
    "create instance of scram client using `apply` method" in {
      val authMechanism = mock[ScramAuthMechanism]
      val entropySource = mock[EntropySource]
      val cache = mock[SaltedPasswordCache]

      SCRAMClient(authMechanism, entropySource, cache) shouldBe an[ClientFirstStep]
    }
  }

}
