package rere.sasl.scram.crypto.entropy.impl

import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.util.{Base64, Base64String, BinaryString, PrintableAndSafe}

class ConstantEntropySource(entropyStr: Base64String, nonceStr: PrintableAndSafe) extends EntropySource {
  def entropy(n: Int): BinaryString = Base64.from(entropyStr)
  def nonce(n: Int): PrintableAndSafe = nonceStr
}
