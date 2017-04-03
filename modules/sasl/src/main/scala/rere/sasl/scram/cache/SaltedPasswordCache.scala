package rere.sasl.scram.cache

import rere.sasl.util.BinaryString

trait SaltedPasswordCache {
  def get(password: String, salt: String, iterations: Int): Option[BinaryString]
  def put(password: String, salt: String, iterations: Int, salted: BinaryString): Unit
}

object SaltedPasswordCache {
  def apply(): SaltedPasswordCache = new impl.ConcurrentSaltedPasswordCache
  def dummy(): SaltedPasswordCache = new impl.StatelessSaltedPasswordCache
}
