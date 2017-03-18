package rere.sasl.scram.client.impl

import akka.util.ByteString
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatest.mockito.MockitoSugar
import rere.sasl._
import rere.sasl.gs2.ChannelBindingFlag.NotSupports
import rere.sasl.scram.client.{AuthError, SaltedPasswordCache}
import rere.sasl.scram.crypto.sha1.ScramSha1AuthMechanismFactory
import rere.sasl.scram.crypto.sha256.ScramSha256AuthMechanismFactory
import rere.sasl.scram.crypto.{NoOpErrorReporter, ScramAuthMechanism}
import rere.sasl.scram.messages._
import rere.sasl.util._

class ClientSecondStepImplTest extends WordSpec with MockitoSugar {

  private val sha1Mechanism = ScramSha1AuthMechanismFactory.getMechanism(new NoOpErrorReporter)
  private val sha256Mechanism = ScramSha256AuthMechanismFactory.getMechanism(new NoOpErrorReporter)

  trait mocks {
    val cache = mock[SaltedPasswordCache]
    when(cache.get(any[String](), any[String](), any[Int]())).thenReturn(None)
  }

  "ClientSecondStepImpl.process" should {
    "process incoming server message and compute proof (rfc 5802)" in new mocks {
      val clientFirstMessage = ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          EscapedString.to("user"),
          new PrintableString("fyko+d2lbbFgONRv9qkxdawL"),
          Nil
        )
      )

      val client = new ClientSecondStepImpl(clientFirstMessage, "pencil", sha1Mechanism, cache)

      val serverFirstMessage = ServerFirstMessage(
        None,
        new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
        new Base64String("QSXCR+Q6sek8bf92"),
        4096,
        Nil
      )

      val Right((msgByteString, nextClient)) = client.process(serverFirstMessage)

      msgByteString shouldBe
        ByteString("c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=")

      nextClient.expectedServerSignature shouldBe
        new Base64String("rmF9pqV8S7suAoZWja4dJRkFsKQ=")
    }

    "process incoming server message and compute proof (rfc 7677)" in new mocks {
      val clientFirstMessage = ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          EscapedString.to("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )

      val client = new ClientSecondStepImpl(clientFirstMessage, "pencil", sha256Mechanism, cache)

      val serverFirstMessage = ServerFirstMessage(
        None,
        new PrintableString("rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"),
        new Base64String("W22ZaJ0SNY7soEsUEjb6gQ=="),
        4096,
        Nil
      )

      val Right((msgByteString, nextClient)) = client.process(serverFirstMessage)

      msgByteString shouldBe
        ByteString("c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=")

      nextClient.expectedServerSignature shouldBe
        new Base64String("6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=")
    }

    "normalize password (not by spec)" in new mocks {
      //values in this test computed for normalized password, then password replaced with not normalized form

      val clientFirstMessage = ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          EscapedString.to("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )

      val password = "～" // not ~ but should be normalized to ~
      val client = new ClientSecondStepImpl(clientFirstMessage, password, sha256Mechanism, cache)

      val serverFirstMessage = ServerFirstMessage(
        None,
        new PrintableString("rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"),
        new Base64String("W22ZaJ0SNY7soEsUEjb6gQ=="),
        4096,
        Nil
      )

      val Right((msgByteString, nextClient)) = client.process(serverFirstMessage)

      msgByteString shouldBe
        ByteString("c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,p=LT3ZBu4t+rgkg6duKgE79P/AyASWqGzvgFZn4h1Wab0=")

      nextClient.expectedServerSignature shouldBe
        new Base64String("h1T0nttwSEJqrMdKDMRkScLdk6jWTDBkHHzz8KQIvrU=")
    }

    "check that server nonce is starts with previously sent client nonce" in new mocks {
      val clientFirstMessage = ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          EscapedString.to("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )

      val client = new ClientSecondStepImpl(clientFirstMessage, "pencil", sha256Mechanism, cache)

      val serverFirstMessage = ServerFirstMessage(
        None,
        new PrintableString("totally_not_what_client_sent"),
        new Base64String("W22ZaJ0SNY7soEsUEjb6gQ=="),
        4096,
        Nil
      )

      val Left(msg) = client.process(serverFirstMessage)

      msg shouldBe AuthError("Validation error: nonce is falsified")
    }

    "raise auth error if auth mechanism generate keys with different length" in new mocks {
      val badAuthMechanism = new ScramAuthMechanism {
        override def hi(password: String, salt: BinaryString, iterations: Int): BinaryString =
          sha256Mechanism.hi(password, salt, iterations)

        override def hmac(key: BinaryString, data: String): BinaryString = {
          if (data == "Client Key") {
            Base64.from(new Base64String("pg/JI9Z+hkSpLRa5btpe9GVrDHJcSEN0viVTVXaZ"))
          } else {
            sha256Mechanism.hmac(key, data)
          }
        }

        override def h(data: BinaryString): BinaryString =
          sha256Mechanism.h(data)

        override def SALT_LENGTH: Int = sha256Mechanism.SALT_LENGTH
        override def SALTED_PASSWORD_LENGTH: Int = sha256Mechanism.SALTED_PASSWORD_LENGTH
        override def CLIENT_KEY_LENGTH: Int = sha256Mechanism.CLIENT_KEY_LENGTH
        override def STORED_KEY_LENGTH: Int = sha256Mechanism.STORED_KEY_LENGTH
        override def CLIENT_SIGNATURE_LENGTH: Int = sha256Mechanism.CLIENT_SIGNATURE_LENGTH
        override def CLIENT_PROOF_LENGTH: Int = sha256Mechanism.CLIENT_PROOF_LENGTH
        override def DEFAULT_ITERATION_COUNT: Int = sha256Mechanism.DEFAULT_ITERATION_COUNT
        override def DEFAULT_NONCE_LENGTH: Int = sha256Mechanism.DEFAULT_NONCE_LENGTH
      }

      val clientFirstMessage = ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          EscapedString.to("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )

      val client = new ClientSecondStepImpl(clientFirstMessage, "pencil", badAuthMechanism, cache)

      val serverFirstMessage = ServerFirstMessage(
        None,
        new PrintableString("rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"),
        new Base64String("W22ZaJ0SNY7soEsUEjb6gQ=="),
        4096,
        Nil
      )

      val Left(msg) = client.process(serverFirstMessage)

      msg shouldBe AuthError("Mismatch of keys length.")
    }

    "try to get salted password from cache and put in cache if not found" in new mocks {
      val clientFirstMessage = ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          EscapedString.to("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )

      val client = new ClientSecondStepImpl(clientFirstMessage, "pencil", sha256Mechanism, cache)

      val serverFirstMessage = ServerFirstMessage(
        None,
        new PrintableString("rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"),
        new Base64String("W22ZaJ0SNY7soEsUEjb6gQ=="),
        4096,
        Nil
      )

      val Right(_) = client.process(serverFirstMessage)

      verify(cache).get("pencil", "W22ZaJ0SNY7soEsUEjb6gQ==", 4096)
      verify(cache).put("pencil", "W22ZaJ0SNY7soEsUEjb6gQ==", 4096, Base64.from(
        new Base64String("xKSVEDI6tPlSysH6mUQZOeeOp01r6B3fcJbodRPcYV0=")
      ))
      verifyNoMoreInteractions(cache)
    }

    "try get salted password from cache and use it if it found" in new mocks {
      when(cache.get(any[String](), any[String](), any[Int]())).thenReturn(Some(Base64.from(new Base64String(
        "PGS8BrhyGs5f2F60qETDRM+yrUL0QYtLg6+76ad46yc="    // key from ~ test
      ))))

      val clientFirstMessage = ClientFirstMessage(
        gs2.Header(NotSupports, None),
        ClientFirstMessageBare(
          None,
          EscapedString.to("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )

      val client = new ClientSecondStepImpl(clientFirstMessage, "pencil", sha256Mechanism, cache)

      val serverFirstMessage = ServerFirstMessage(
        None,
        new PrintableString("rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"),
        new Base64String("W22ZaJ0SNY7soEsUEjb6gQ=="),
        4096,
        Nil
      )

      val Right((msgByteString, nextClient)) = client.process(serverFirstMessage)

      msgByteString shouldBe
        ByteString("c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,p=LT3ZBu4t+rgkg6duKgE79P/AyASWqGzvgFZn4h1Wab0=")

      nextClient.expectedServerSignature shouldBe
        new Base64String("h1T0nttwSEJqrMdKDMRkScLdk6jWTDBkHHzz8KQIvrU=")

      verify(cache).get("pencil", "W22ZaJ0SNY7soEsUEjb6gQ==", 4096)
      verifyNoMoreInteractions(cache)
    }
  }

}
