package rere.sasl.scram.storage.impl

import java.util.concurrent.ConcurrentHashMap

import rere.sasl.scram.crypto
import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.scram.server.AuthData
import rere.sasl.scram.storage.SaltedPasswordStorage

class SaltedPasswordStorageInMemoryImpl(
    authMechanism: ScramAuthMechanism,
    entropySource: EntropySource
  ) extends SaltedPasswordStorage {

  private val volume: ConcurrentHashMap[String, AuthData] = new ConcurrentHashMap()

  def store(username: String, password: String): Unit = {
    val salt = entropySource.entropy(authMechanism.SALT_LENGTH)
    val iterationCount = authMechanism.DEFAULT_ITERATION_COUNT

    val saltedPassword = authMechanism.hi(
      crypto.normalize(password),
      salt,
      iterationCount
    )

    val clientKey = authMechanism.hmac(saltedPassword, crypto.CLIENT_KEY)

    val storedKey = authMechanism.h(clientKey)

    val authData = AuthData(username, saltedPassword, salt, iterationCount, isReal = true, clientKey, storedKey)

    volume.put(username, authData)
    ()
  }

  def getAuthData(username: String): AuthData = {
    volume.get(username) match {
      case authData: AuthData => authData
      case _ =>
        val saltedPassword = entropySource.entropy(authMechanism.SALTED_PASSWORD_LENGTH)
        val salt = entropySource.entropy(authMechanism.SALT_LENGTH)
        val iterationCount = authMechanism.DEFAULT_ITERATION_COUNT
        val clientKey = entropySource.entropy(authMechanism.CLIENT_KEY_LENGTH)
        val storedKey = entropySource.entropy(authMechanism.STORED_KEY_LENGTH)

        AuthData(username, saltedPassword, salt, iterationCount, isReal = false, clientKey, storedKey)
    }
  }
}
