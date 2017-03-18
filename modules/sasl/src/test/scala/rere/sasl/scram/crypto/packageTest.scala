package rere.sasl.scram.crypto

import org.scalatest.{Matchers, WordSpec}
import rere.sasl.util.{Base64, Base64String}

class packageTest extends WordSpec with Matchers {

  "normalize" should {
    "replace some symbols to more commonly used alternatives" in {
      normalize("～") shouldBe "~"
    }
  }

  "xor" should {
    "compute xor of two binary strings" in {
      val a = Base64.from(new Base64String("8wZyS9EoWv+s+PKiK+vf4mrqgKzargMxUk9PLJq4JTk="))
      val b = Base64.from(new Base64String("3jurTT8FoEeIe1XMAerkFpUqSKhMBm/e0hkozofuTIQ="))
      val expected = Base64.from(new Base64String("LT3ZBu4t+rgkg6duKgE79P/AyASWqGzvgFZn4h1Wab0="))

      xor(a, b).right.get.toVector shouldBe expected.toVector
      xor(b, a).right.get.toVector shouldBe expected.toVector
    }

    "return error message if the size of the two binary strings are not equal" in {
      val a = Base64.from(new Base64String("8wZyS9Eo"))
      val b = Base64.from(new Base64String("3jur"))

      xor(a, b) shouldBe Left("Mismatch of keys length.")
    }
  }

}
