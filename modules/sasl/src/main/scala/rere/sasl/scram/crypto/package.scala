package rere.sasl.scram

import java.text.Normalizer

import rere.sasl.util.BinaryString

package object crypto {
  val CLIENT_KEY = "Client Key"
  val SERVER_KEY = "Server Key"

  // Naive implementation, but it's better than nothing
  def normalize(str: String): String = {
    Normalizer.normalize(str, Normalizer.Form.NFKC)
  }

  def xor(a: BinaryString, b: BinaryString): Either[String, BinaryString] = {
    if (a.length != b.length) {
      Left("Mismatch of keys length.")
    } else {
      val result = new Array[Byte](a.length)
      var i = result.length - 1
      while (i >= 0) {
        result(i) = (a(i) ^ b(i)).toByte
        i -= 1
      }
      Right(result)
    }
  }
}
