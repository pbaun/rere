package rere.sasl.util

object UTF8 {
  import java.nio.charset.StandardCharsets

  def to(str: String): BinaryString = {
    str.getBytes(StandardCharsets.UTF_8)
  }

  def from(bytes: BinaryString): String = {
    new String(bytes, StandardCharsets.UTF_8)
  }
}
