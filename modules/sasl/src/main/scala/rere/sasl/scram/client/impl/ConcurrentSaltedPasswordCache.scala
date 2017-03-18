package rere.sasl.scram.client.impl

import java.util.concurrent.ConcurrentHashMap

import rere.sasl.scram.client.SaltedPasswordCache
import rere.sasl.util.BinaryString

class ConcurrentSaltedPasswordCache extends SaltedPasswordCache {

  case class Entry(password: String, salt: String, iterations: Int)
  private val state = new ConcurrentHashMap[Entry, BinaryString]

  override def get(password: String, salt: String, iterations: Int): Option[BinaryString] = {
    Option(state.get(Entry(password, salt, iterations)))
  }

  override def put(password: String, salt: String, iterations: Int, salted: BinaryString): Unit = {
    state.put(Entry(password, salt, iterations), salted)
    ()
  }
}
