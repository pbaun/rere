package rere.sasl.scram.client.impl

import rere.sasl.scram.client.SaltedPasswordCache
import rere.sasl.util.BinaryString

class StatelessSaltedPasswordCache extends SaltedPasswordCache {
  override def get(password: String, salt: String, iterations: Int): Option[BinaryString] = None
  override def put(password: String, salt: String, iterations: Int, salted: BinaryString): Unit = ()
}
