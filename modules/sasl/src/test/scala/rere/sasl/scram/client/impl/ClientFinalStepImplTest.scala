package rere.sasl.scram.client.impl

import org.scalatest.Matchers._
import org.scalatest.WordSpec
import rere.sasl.scram.client.AuthError
import rere.sasl.scram.messages.ServerErrorType.InvalidProof
import rere.sasl.scram.messages.{ClientFinalMessage, ClientFinalMessageWithoutProof, ServerError, ServerFinalMessage}
import rere.sasl.util.{Base64String, PrintableString}

class ClientFinalStepImplTest extends WordSpec {

  "ClientFinalStepImpl" should {
    "accept server message with expected signature (rfc 7677)" in {
      val client = new ClientFinalStepImpl(
        ClientFinalMessage(
          ClientFinalMessageWithoutProof(
            new Base64String("biws"),
            new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
            Nil
          ),
          new Base64String("v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=")
        ),
        new Base64String("6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=")
      )

      val serverFinalMessage = ServerFinalMessage(
        Right(new Base64String("6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=")),
        Nil
      )

      val result = client.process(serverFinalMessage)

      result shouldBe Right("OK")
    }

    "return auth error if received verification signature not match computed one" in {
      val client = new ClientFinalStepImpl(
        ClientFinalMessage(
          ClientFinalMessageWithoutProof(
            new Base64String("biws"),
            new PrintableString("rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"),
            Nil
          ),
          new Base64String("dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=")
        ),
        new Base64String("6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=")
      )

      val serverFinalMessage = ServerFinalMessage(
        Right(new Base64String("rmF9pqV8S7suAoZWja4dJRkFsKQ=")),
        Nil
      )

      val result = client.process(serverFinalMessage)

      result shouldBe
        Left(AuthError("Verification failed: rmF9pqV8S7suAoZWja4dJRkFsKQ= != 6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4="))
    }

    "return error if server sent error message" in {
      val client = new ClientFinalStepImpl(
        ClientFinalMessage(
          ClientFinalMessageWithoutProof(
            new Base64String("biws"),
            new PrintableString("rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"),
            Nil
          ),
          new Base64String("dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=")
        ),
        new Base64String("6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=")
      )

      val serverFinalMessage = ServerFinalMessage(
        Left(ServerError(InvalidProof)),
        Nil
      )

      val result = client.process(serverFinalMessage)

      result shouldBe
        Left(AuthError("Server error: InvalidProof"))
    }
  }
}
