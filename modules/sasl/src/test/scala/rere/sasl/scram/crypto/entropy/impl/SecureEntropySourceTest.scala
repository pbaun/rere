package rere.sasl.scram.crypto.entropy.impl

import org.scalatest.{Matchers, WordSpec}

class SecureEntropySourceTest extends WordSpec with Matchers {

  "SecureEntropySource.entropy" should {
    "return random binary strings" in {
      val probe1 = SecureEntropySource.entropy(8)
      val probe2 = SecureEntropySource.entropy(8)

      probe1 should not be probe2
    }
  }

  "SecureEntropySource.nonce" should {
    "return random printable strings" in {
      val probe1 = SecureEntropySource.nonce(8)
      val probe2 = SecureEntropySource.nonce(8)

      probe1 should not be probe2
    }
  }

}
