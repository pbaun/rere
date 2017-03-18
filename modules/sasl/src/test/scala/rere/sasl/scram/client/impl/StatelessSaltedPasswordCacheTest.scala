package rere.sasl.scram.client.impl

import org.scalatest.{Matchers, WordSpec}

class StatelessSaltedPasswordCacheTest extends WordSpec with Matchers {

  "StatelessSaltedPasswordCache" should {
    "not cache salted password" in {
      val cache = new StatelessSaltedPasswordCache()

      cache.get("secret", "salt", 4096) shouldBe None
      cache.put("secret", "salt", 4096, Array[Byte](97, 98, 99))
      cache.get("secret", "salt", 4096) shouldBe None
    }
  }
}
