package rere.sasl.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class NoCommaStringTest extends FlatSpec {

  behavior of "NoCommaString"

  it should "provide correct equals implementation" in {
    new NoCommaString("abc") shouldBe new NoCommaString("abc")
  }

  it should "provide correct hashCode implementation" in {
    new NoCommaString("abc").hashCode() shouldBe new NoCommaString("abc").hashCode()
  }

  it should "return original string as result of toString method" in {
    new NoCommaString("abc").toString shouldBe "abc"
  }
}
