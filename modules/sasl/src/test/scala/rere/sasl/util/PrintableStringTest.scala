package rere.sasl.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class PrintableStringTest extends FlatSpec {

  behavior of "PrintableString"

  it should "provide correct equals implementation" in {
    new PrintableString("abc") shouldBe new PrintableString("abc")
  }

  it should "provide correct hashCode implementation" in {
    new PrintableString("abc").hashCode() shouldBe new PrintableString("abc").hashCode()
  }

  it should "return original string as result of toString method" in {
    new PrintableString("abc").toString shouldBe "abc"
  }

  it should "create printable string from regular string using `apply` method" in {
    PrintableString("\u0020!|+,-|~\u007f").toString() shouldBe "!|+-|~"
  }
}
