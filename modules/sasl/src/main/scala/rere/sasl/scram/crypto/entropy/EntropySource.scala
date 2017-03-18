package rere.sasl.scram.crypto.entropy

import rere.sasl.util.{BinaryString, PrintableAndSafe}

trait EntropySource {
  def entropy(entropyBytes: Int): BinaryString
  def nonce(entropyBytes: Int): PrintableAndSafe
}
