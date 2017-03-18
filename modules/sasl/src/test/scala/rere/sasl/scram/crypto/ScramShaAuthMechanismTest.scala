package rere.sasl.scram.crypto

import org.scalatest.{Matchers, WordSpec}
import rere.sasl.scram.crypto.sha1.ScramSha1AuthMechanismFactory
import rere.sasl.util.{Base64, Base64String}

class ScramShaAuthMechanismTest extends WordSpec with Matchers {

  private val mechanism = ScramSha1AuthMechanismFactory.getMechanism(new NoOpErrorReporter)

  "ScramShaAuthMechanism.hi" should {
    "work" in {
      val result = mechanism.hi("pencil", Base64.from(new Base64String("fyko+d2lbbFgONRv9qkxdawL")), 4096)
      val expected = new Base64String("MbwZVd4rBTSTjtaTk+FD4nKmqxQ=")

      Base64.to(result) shouldBe expected
    }

    "throw exception" in {
      intercept[NullPointerException] {
        mechanism.hi("pencil", null, 4096)
      }.getMessage should include("salt")
    }
  }

  "ScramShaAuthMechanism.hmac" should {
    "work" in {
      val result = mechanism.hmac(Base64.from(new Base64String("HZbuOlKbWl+eR8AfIposuKbhX30=")), "Client Key")
      val expected = new Base64String("4jTEe/bDZpbdbYUrmaqiuiZVVyg=")

      Base64.to(result) shouldBe expected
    }

    "throw exception" in {
      intercept[IllegalArgumentException] {
        mechanism.hmac(null, "abc")
      }
    }
  }

  "ScramShaAuthMechanism.h" should {
    "work" in {
      val result = mechanism.h(Base64.from(new Base64String("4jTEe/bDZpbdbYUrmaqiuiZVVyg=")))
      val expected = new Base64String("6dlGYMOdZcOPutkcNY8U2g7vK9Y=")

      Base64.to(result) shouldBe expected
    }

    "throw exception" in {
      intercept[NullPointerException] {
        mechanism.h(null)
      }
    }
  }

}
