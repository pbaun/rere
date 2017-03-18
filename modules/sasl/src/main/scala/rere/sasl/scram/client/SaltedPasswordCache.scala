package rere.sasl.scram.client

import rere.sasl.util.BinaryString

trait SaltedPasswordCache {
  def get(password: String, salt: String, iterations: Int): Option[BinaryString]
  def put(password: String, salt: String, iterations: Int, salted: BinaryString): Unit
}
