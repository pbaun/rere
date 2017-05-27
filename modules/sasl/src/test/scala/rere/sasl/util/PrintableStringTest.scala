package rere.sasl.util

import org.scalatest.{Matchers, WordSpec}

class PrintableStringTest extends WordSpec with Matchers {

  "PrintableString object" should {
    "create printable string from regular string using `apply` method" in {
      PrintableString("\u0020!|+,-|~\u007f").toString() shouldBe "!|+-|~"
    }
  }

}
