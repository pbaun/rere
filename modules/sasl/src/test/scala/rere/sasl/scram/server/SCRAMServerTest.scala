package rere.sasl.scram.server

import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.scram.storage.SaltedPasswordStorage

class SCRAMServerTest extends WordSpec with Matchers with MockFactory {

  "SCRAMServer object" should {
    "create instance of scram server using `apply` method" in {
      val authMechanism = mock[ScramAuthMechanism]
      val entropySource = mock[EntropySource]
      val storage = mock[SaltedPasswordStorage]

      SCRAMServer(authMechanism, entropySource, storage) shouldBe an[ServerFirstStep]
    }
  }

}
