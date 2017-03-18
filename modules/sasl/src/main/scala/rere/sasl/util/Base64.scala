package rere.sasl.util

object Base64 {
  val encoder = java.util.Base64.getEncoder
  val decoder = java.util.Base64.getDecoder

  def to(data: BinaryString): Base64String = {
    new Base64String(UTF8.from(encoder.encode(data)))
  }

  def from(encoded: Base64String): BinaryString = {
    decoder.decode(encoded.str)
  }
}