package rere.sasl.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class EscapedStringTest extends FlatSpec {

  behavior of "EscapedString"

  it should "compute same hash code for string with upper case replacement and string with lower case replacement" in {
    val withUpperCaseReplacementHashCode = new EscapedString("abc=2Cdef").hashCode()
    val withLowerCaseReplacementHashCode = new EscapedString("abc=2cdef").hashCode()

    withUpperCaseReplacementHashCode shouldBe withLowerCaseReplacementHashCode
  }

  it should "treat string with upper case replacement same as string with lower case replacement" in {
    val withUpperCaseReplacement = new EscapedString("abc=2Cdef")
    val withLowerCaseReplacement = new EscapedString("abc=2cdef")

    withUpperCaseReplacement shouldBe withLowerCaseReplacement
  }

  it should "treat regular string with same content as not equal" in {
    new EscapedString("abc").equals("abc") shouldBe false
  }

  it should "return unescaped value as result of toString method" in {
    new EscapedString("abc=2Cdef").toString shouldBe "abc,def"
  }

  it should "encode regular string to safe" in {
    EscapedString.to("abc,def=ghi") shouldBe new EscapedString("abc=2Cdef=3Dghi")
    EscapedString.to("абв,ГДЕ=\uD83D\uDE00") shouldBe new EscapedString("абв=2CГДЕ=3D\uD83D\uDE00")
    EscapedString.to("абв,ГДЕ=\uD83D=\uDE00") shouldBe new EscapedString("абв=2CГДЕ=3D\uD83D=3D\uDE00")
  }

  it should "encode all symbols in one pass, not just one per pass" in {
    EscapedString.to(",,,") shouldBe new EscapedString("=2C=2C=2C")
    EscapedString.to("===") shouldBe new EscapedString("=3D=3D=3D")
  }

  it should "start encoding from replace to =3D and then replace to =2C" in {
    EscapedString.to(",=") shouldBe new EscapedString("=2C=3D")
    EscapedString.to(",=") should not be new EscapedString("=3D2C=3D")
  }

  it should "decode safe strings to regular" in {
    EscapedString.from(new EscapedString("abc=2Cdef=3Dghi")) shouldBe "abc,def=ghi"
    EscapedString.from(new EscapedString("abc=2cdef=3dghi")) shouldBe "abc,def=ghi"
    EscapedString.from(new EscapedString("абв=2CГДЕ=3D\uD83D\uDE00")) shouldBe "абв,ГДЕ=\uD83D\uDE00"
    EscapedString.from(new EscapedString("абв=2CГДЕ=3D\uD83D=3d\uDE00")) shouldBe "абв,ГДЕ=\uD83D=\uDE00"
  }

  it should "decode all symbols in one pass" in {
    EscapedString.from(new EscapedString("=2C=2C=2C")) shouldBe ",,,"
    EscapedString.from(new EscapedString("=3D=3D=3D")) shouldBe "==="
    EscapedString.from(new EscapedString("=3D3D")) shouldBe "=3D"
  }

  it should "decode starting from replace of =2C and then replace =3D" in {
    EscapedString.from(new EscapedString("=3D2C")) shouldBe "=2C"
    EscapedString.from(new EscapedString("=3D2C")) should not be ","
  }

  it should "decode lower case alternatives" in {
    EscapedString.from(new EscapedString("=2c=2c")) shouldBe ",,"
    EscapedString.from(new EscapedString("=3d=3d")) shouldBe "=="
    EscapedString.from(new EscapedString("=2c=3d")) shouldBe ",="
    EscapedString.from(new EscapedString("=3d=2c")) shouldBe "=,"
  }

  it should "throw exception if passes sequence is not valid" in {
    intercept[IllegalArgumentException] {
      EscapedString.from(new EscapedString("abc=4e=3D"))
    }.getMessage should include("=4")

    intercept[IllegalArgumentException] {
      EscapedString.from(new EscapedString("abc=2d=3D"))
    }.getMessage should include("=2d")

    intercept[IllegalArgumentException] {
      EscapedString.from(new EscapedString("abc=3c=3D"))
    }.getMessage should include("=3c")
  }
}
