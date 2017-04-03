package rere.sasl.scram.storage

import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.scram.server.AuthData

trait SaltedPasswordStorage {
  def store(username: String, password: String): Unit
  def getAuthData(username: String): AuthData
}

object SaltedPasswordStorage {
  def apply(
    authMechanism: ScramAuthMechanism,
    entropySource: EntropySource
  ): SaltedPasswordStorage = {
    new impl.SaltedPasswordStorageInMemoryImpl(authMechanism, entropySource)
  }
}
