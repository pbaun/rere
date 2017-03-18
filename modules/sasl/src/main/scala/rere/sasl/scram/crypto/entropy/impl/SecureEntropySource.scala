package rere.sasl.scram.crypto.entropy.impl

import java.security.SecureRandom

import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.util.{Base64, BinaryString, PrintableAndSafe}

object SecureEntropySource extends EntropySource {
  private val secureRandom = new SecureRandom()

  override def entropy(entropyBytes: Int): BinaryString = {
    val buffer = new Array[Byte](entropyBytes)
    secureRandom.nextBytes(buffer)
    buffer
  }

  override def nonce(entropyBytes: Int): PrintableAndSafe = {
    Base64.to(entropy(entropyBytes))
  }
}
