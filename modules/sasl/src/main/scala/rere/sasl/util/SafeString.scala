package rere.sasl.util

import scala.annotation.switch

//TODO: private constructors + companion objects

/**
  * String without ',' that can be safely sent as value in attr-val pair
  */
sealed trait SafeString extends Any

/**
  * Escaped string without ',' and NUL
  * @param str - representation
  */
final class EscapedString private[sasl](private[sasl] val str: String) extends AnyVal with SafeString

/**
  * String without comma
  * @param str - representation
  */
final class NoCommaString private[sasl](private[sasl] val str: String) extends AnyVal with SafeString

/**
  * String with only printable chars and without comma
  */
sealed trait PrintableAndSafe extends Any with SafeString

/**
  * Naive implementation of string with only printable chars and without comma
  * @param str - representation
  */
final class PrintableString private[sasl](private[sasl] val str: String) extends AnyVal with PrintableAndSafe {
  override def toString(): String = str
}

/**
  * Base64 encoded string. Also with only printable chars and without comma
  * @param str - representation
  */
final class Base64String private[sasl](private[sasl] val str: String) extends AnyVal with PrintableAndSafe {
  override def toString(): String = str
}

object EscapedString {

  private val expectedExpansion = 1.2

  def to(str: String): EscapedString = {
    val builder = new StringBuilder((str.length * expectedExpansion).toInt)
    str foreach {
      case ',' => builder.append("=2C")
      case '=' => builder.append("=3D")
      case ch => builder.append(ch)
    }

    new EscapedString(builder.toString())
  }

  def from(safe: EscapedString): String = {
    val s = safe.str
    val builder = new StringBuilder(s.length)

    val end = s.length - 2
    var i = 0
    while(i < end) {
      val ch1 = s.charAt(i)

      if (ch1 == '=') {
        val ch2 = s.charAt(i + 1)

        (ch2: @switch) match {
          case '2' =>
            val ch3 = s.charAt(i + 2)
            if (ch3 == 'C') {
              builder.append(',')
              i += 3
            } else {
              builder.append(ch1)
              i += 1
            }

          case '3' =>
            val ch3 = s.charAt(i + 2)
            if (ch3 == 'D') {
              builder.append('=')
              i += 3
            } else {
              builder.append(ch1)
              i += 1
            }

          case c =>
            throw new IllegalArgumentException(s"Invalid character sequence: '=$c'")
        }
      } else {
        builder.append(ch1)
        i += 1
      }
    }

    while(i < s.length) {
      builder.append(s.charAt(i))
      i += 1
    }

    builder.toString()
  }
}
