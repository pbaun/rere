package rere.sasl.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class Base64StringTest extends FlatSpec {

  behavior of "Base64String"

  it should "provide correct equals implementation" in {
    new Base64String("abc") shouldBe new Base64String("abc")
    new Base64String("abc").equals("abc") shouldBe false
  }

  it should "provide correct hashCode implementation" in {
    new Base64String("abc").hashCode() shouldBe new Base64String("abc").hashCode()
  }

  it should "return original string as result of toString method" in {
    new Base64String("abcd").toString shouldBe "abcd"
  }
}
