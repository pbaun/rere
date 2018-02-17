package rere.sasl.util

import scala.annotation.switch

/**
  * String without ',' that can be safely sent as value in attr-val pair
  */
sealed trait SafeString

/**
  * Escaped string without ',' and NUL. ',' and '=' replaced with "=2C" and "=3D".
  * @param str - representation
  */
final class EscapedString private[sasl](private[sasl] val str: String) extends SafeString {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: EscapedString => EscapedString.from(this) == EscapedString.from(that)
      case _ => false
    }
  }

  override def hashCode(): Int = EscapedString.from(this).hashCode()

  override def toString: String = EscapedString.from(this)
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
            if (ch3 == 'C' || ch3 == 'c') {
              builder.append(',')
              i += 3
            } else {
              throw new IllegalArgumentException(s"Invalid character sequence: '$ch1$ch2$ch3'")
            }

          case '3' =>
            val ch3 = s.charAt(i + 2)
            if (ch3 == 'D' || ch3 == 'd') {
              builder.append('=')
              i += 3
            } else {
              throw new IllegalArgumentException(s"Invalid character sequence: '$ch1$ch2$ch3'")
            }

          case _ =>
            throw new IllegalArgumentException(s"Invalid character sequence: '$ch1$ch2'")
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

/**
  * String without comma
  * @param str - representation
  */
final class NoCommaString private[sasl](private[sasl] val str: String) extends SafeString {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: NoCommaString => this.str == that.str
      case _ => false
    }
  }

  override def hashCode(): Int = str.hashCode()

  override def toString: String = str
}

/**
  * String with only printable chars and without commas
  */
sealed trait PrintableAndSafe extends SafeString

/**
  * Naive implementation of string with only printable chars and without commas
  * @param str - representation
  */
final class PrintableString private[sasl](private[sasl] val str: String) extends PrintableAndSafe {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: PrintableString => this.str == that.str
      case _ => false
    }
  }

  override def hashCode(): Int = str.hashCode()

  override def toString: String = str
}

object PrintableString {
  private[rere] def apply(str: String): PrintableString = {
    val filtered = str.filter { char =>
      ('\u0021' <= char && char <= '\u002b') || ('\u002d' <= char && char <= '\u007e')
    }
    new PrintableString(filtered)
  }
}

/**
  * Base64 encoded string. Also with only printable chars and without commas
  * @param str - representation
  */
final class Base64String private[sasl](private[sasl] val str: String) extends PrintableAndSafe {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Base64String => this.str == that.str
      case _ => false
    }
  }

  override def hashCode(): Int = str.hashCode()

  override def toString: String = str
}

