package rere.sasl.scram.cache

import org.scalatest.{Inside, Matchers, WordSpec}

class SaltedPasswordCacheTest extends WordSpec with Matchers with Inside {

  "SaltedPasswordCache object" should {
    "create stateful instance of cache using `apply` method" in {
      val cache = SaltedPasswordCache()
      cache.put("qwerty", "salt", 4096, Array[Byte](97, 98, 99))
      inside(cache.get("qwerty", "salt", 4096)) {
        case Some(salted) =>
          salted.toVector shouldBe Vector[Byte](97, 98, 99)
      }
    }

    "crate stateless instance of cache using `dummy` method" in {
      val cache = SaltedPasswordCache.dummy()
      cache.put("qwerty", "salt", 4096, Array[Byte](97, 98, 99))
      cache.get("qwerty", "salt", 4096) shouldBe None
    }
  }

}
