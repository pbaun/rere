package rere.sasl.scram.storage

import org.scalatest.{Matchers, WordSpec}
import rere.sasl.scram.crypto.NoOpErrorReporter
import rere.sasl.scram.crypto.entropy.impl.SecureEntropySource
import rere.sasl.scram.crypto.sha256.ScramSha256AuthMechanismFactory

class SaltedPasswordStorageTest extends WordSpec with Matchers {

  "SaltedPasswordStorage object" should {
    "create stateful instance of password storage using `apply` method" in {
      val authMechanism = ScramSha256AuthMechanismFactory.getMechanism(new NoOpErrorReporter)
      val entropySource = SecureEntropySource

      val storage = SaltedPasswordStorage(authMechanism, entropySource)

      storage.store("admin", "qwerty")

      storage.getAuthData("admin").isReal shouldBe true

      storage.getAuthData("root").isReal shouldBe false
    }
  }

}
