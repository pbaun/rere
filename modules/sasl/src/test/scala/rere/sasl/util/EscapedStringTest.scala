package rere.sasl.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class EscapedStringTest extends FlatSpec {

  behavior of "EscapedString"

  it should "encode regular string to safe" in {
    EscapedString.to("abc,def=ghi") shouldBe new EscapedString("abc=2Cdef=3Dghi")
    //TODO: unicode tests
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
    //TODO: unicode tests
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

  it should "not decode lower case alternatives" in {
    EscapedString.from(new EscapedString("=2c=2c")) shouldBe "=2c=2c"
    EscapedString.from(new EscapedString("=3d=3d")) shouldBe "=3d=3d"
    EscapedString.from(new EscapedString("=2c=3d")) shouldBe "=2c=3d"
    EscapedString.from(new EscapedString("=3d=2c")) shouldBe "=3d=2c"
  }

  it should "throw exception if passes sequence is not valid" in {
    intercept[IllegalArgumentException] {
      EscapedString.from(new EscapedString("abc=4e=3D"))
    }.getMessage should include("=4")
  }
}
