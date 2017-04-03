package rere.sasl.scram.cache.impl

import org.scalatest.{Matchers, WordSpec}

class ConcurrentSaltedPasswordCacheTest extends WordSpec with Matchers {

  "ConcurrentSaltedPasswordCache" should {
    "cache salted passwords" in {
      val cache = new ConcurrentSaltedPasswordCache()

      cache.get("secret", "salt", 4096) shouldBe None
      cache.put("secret", "salt", 4096, Array[Byte](97, 98, 99))
      cache.get("secret", "salt", 4096).map(_.toVector) shouldBe Some(Vector[Byte](97, 98, 99))

      cache.put("secret", "salt", 4096, Array[Byte](97, 98, 100))
      cache.get("secret", "salt", 4096).map(_.toVector) shouldBe Some(Vector[Byte](97, 98, 100))
    }
  }

}
