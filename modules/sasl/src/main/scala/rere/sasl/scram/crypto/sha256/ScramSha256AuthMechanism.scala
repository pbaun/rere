package rere.sasl.scram.crypto.sha256

import javax.crypto.SecretKeyFactory

import rere.sasl.scram.crypto.{ErrorReporter, ScramShaAuthMechanism}

class ScramSha256AuthMechanism private[scram](errorReporter: ErrorReporter) extends ScramShaAuthMechanism {
  val MAC_ALGORITHM = "HmacSHA256"
  val PBKDF2_KEY_LENGTH = 256
  val PBKDF2_ALGORITHM = "PBKDF2WithHmacSHA256"
  val HASH_FUNCTION_NAME = "SHA-256"

  // This operation can take some time
  override val factory: SecretKeyFactory = SecretKeyFactory.getInstance(PBKDF2_ALGORITHM)
  override def log(throwable: Throwable): Unit = errorReporter.onError(throwable)

  val SALT_LENGTH = 32
  val SALTED_PASSWORD_LENGTH = 32
  val CLIENT_KEY_LENGTH = 32
  val STORED_KEY_LENGTH = 32
  val CLIENT_SIGNATURE_LENGTH = 32
  val CLIENT_PROOF_LENGTH = 32
  val DEFAULT_ITERATION_COUNT = 4096
  val DEFAULT_NONCE_LENGTH = 18
}
