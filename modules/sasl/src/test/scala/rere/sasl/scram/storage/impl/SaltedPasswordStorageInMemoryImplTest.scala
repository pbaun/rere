package rere.sasl.scram.storage.impl

import org.scalatest.{FlatSpec, Matchers}
import rere.sasl.scram.crypto.NoOpErrorReporter
import rere.sasl.scram.crypto.entropy.impl.ConstantEntropySource
import rere.sasl.scram.crypto.sha1.ScramSha1AuthMechanismFactory
import rere.sasl.util.{Base64, Base64String, PrintableString}

class SaltedPasswordStorageInMemoryImplTest extends FlatSpec with Matchers {

  trait mocks {
    val randomBase64String = new Base64String("QSXCR+Q6sek8bf92")
    val randomBinaryString = Base64.from(randomBase64String)

    val saltEntropySource = new ConstantEntropySource(
      entropyStr = randomBase64String,
      nonceStr = new PrintableString("fyko+d2lbbFgONRv9qkxdawL")
    )
    val mechanism = ScramSha1AuthMechanismFactory.getMechanism(new NoOpErrorReporter)
    val storage = new SaltedPasswordStorageInMemoryImpl(mechanism, saltEntropySource)
  }

  behavior of "SaltedPasswordStorageInMemoryImpl"

  it should "store password in salted form" in new mocks {
    storage.store("user", "qwerty")

    storage.getAuthData("user").isReal shouldBe true
  }

  it should "return AuthData with isReal=true flag if user data already stored in storage" in new mocks {
    storage.store("user", "qwerty")

    val authData = storage.getAuthData("user")
    authData.username shouldBe "user"
    authData.saltedPassword shouldBe Base64.from(new Base64String("YRXzCgmFKGogXWgyxR8uogR/IIU="))
    authData.salt shouldBe randomBinaryString
    authData.i shouldBe mechanism.DEFAULT_ITERATION_COUNT
    authData.isReal shouldBe true
    authData.clientKey shouldBe Base64.from(new Base64String("01xnJxKPU6ddPdP3WQAddxlNcIs="))
    authData.storedKey shouldBe Base64.from(new Base64String("k5bhbzwYzvBHGorMsqJ0oJQ9G4w="))
  }

  it should "return AuthData with isReal=false flag and random values if user data not stored in storage" in new mocks {
    val authData = storage.getAuthData("user")
    authData.username shouldBe "user"
    authData.saltedPassword shouldBe randomBinaryString
    authData.salt shouldBe randomBinaryString
    authData.i shouldBe mechanism.DEFAULT_ITERATION_COUNT
    authData.isReal shouldBe false
    authData.clientKey shouldBe randomBinaryString
    authData.storedKey shouldBe randomBinaryString
  }

}
