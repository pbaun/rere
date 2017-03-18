package rere.sasl.scram.crypto.sha1

import javax.crypto.SecretKeyFactory

import rere.sasl.scram.crypto.{ErrorReporter, ScramShaAuthMechanism}

/**
  * Docs: https://docs.oracle.com/javase/8/docs/technotes/guides/security/StandardNames.html
  */
class ScramSha1AuthMechanism private[scram](errorReporter: ErrorReporter) extends ScramShaAuthMechanism {
  val MAC_ALGORITHM = "HmacSHA1"
  val PBKDF2_KEY_LENGTH = 160
  val PBKDF2_ALGORITHM = "PBKDF2WithHmacSHA1"
  val HASH_FUNCTION_NAME = "SHA-1"

  // This operation can take some time
  override val factory: SecretKeyFactory = SecretKeyFactory.getInstance(PBKDF2_ALGORITHM)
  override def log(throwable: Throwable): Unit = errorReporter.onError(throwable)

  val SALT_LENGTH = 20
  val SALTED_PASSWORD_LENGTH = 20
  val CLIENT_KEY_LENGTH = 20
  val STORED_KEY_LENGTH = 20
  val CLIENT_SIGNATURE_LENGTH = 20
  val CLIENT_PROOF_LENGTH = 20
  val DEFAULT_ITERATION_COUNT = 4096
  val DEFAULT_NONCE_LENGTH = 18
}
