package rere.sasl.scram.crypto

import java.security.MessageDigest
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}
import javax.crypto.{Mac, SecretKeyFactory}

import rere.sasl.util.{BinaryString, UTF8}

import scala.util.control.NonFatal

trait ScramShaAuthMechanism extends ScramAuthMechanism {
  def MAC_ALGORITHM: String
  def PBKDF2_KEY_LENGTH: Int
  def PBKDF2_ALGORITHM: String
  def HASH_FUNCTION_NAME: String

  def factory: SecretKeyFactory
  def log(throwable: Throwable): Unit

  final def hi(password: String, salt: BinaryString, iterations: Int): BinaryString = {
    try {
      val keySpec = new PBEKeySpec(password.toCharArray, salt, iterations, PBKDF2_KEY_LENGTH)
      factory.generateSecret(keySpec).getEncoded
    } catch {
      case NonFatal(ex) =>
        log(ex)
        throw ex
    }
  }

  final def hmac(key: BinaryString, data: String): BinaryString = {
    try {
      val mac = Mac.getInstance(MAC_ALGORITHM)
      val secretKey = new SecretKeySpec(key, MAC_ALGORITHM)
      mac.init(secretKey)
      mac.doFinal(UTF8.to(data))
    } catch {
      case NonFatal(ex) =>
        log(ex)
        throw ex
    }
  }

  final def h(data: BinaryString): BinaryString = {
    try {
      MessageDigest.getInstance(HASH_FUNCTION_NAME).digest(data)
    } catch {
      case NonFatal(ex) =>
        log(ex)
        throw ex
    }
  }
}
