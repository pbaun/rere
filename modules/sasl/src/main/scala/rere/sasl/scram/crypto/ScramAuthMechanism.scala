package rere.sasl.scram.crypto

import rere.sasl.util.BinaryString

trait ScramAuthMechanism {
  def hi(password: String, salt: BinaryString, iterations: Int): BinaryString
  def hmac(key: BinaryString, data: String): BinaryString
  def h(data: BinaryString): BinaryString

  def SALT_LENGTH: Int
  def SALTED_PASSWORD_LENGTH: Int
  def CLIENT_KEY_LENGTH: Int
  def STORED_KEY_LENGTH: Int
  def CLIENT_SIGNATURE_LENGTH: Int
  def CLIENT_PROOF_LENGTH: Int
  def DEFAULT_ITERATION_COUNT: Int
  def DEFAULT_NONCE_LENGTH: Int
}
