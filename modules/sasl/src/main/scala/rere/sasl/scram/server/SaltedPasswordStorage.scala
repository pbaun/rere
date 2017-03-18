package rere.sasl.scram.server

trait SaltedPasswordStorage {
  def store(username: String, password: String): Unit
  def getAuthData(username: String): AuthData
}
