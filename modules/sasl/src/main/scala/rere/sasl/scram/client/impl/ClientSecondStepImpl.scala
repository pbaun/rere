package rere.sasl.scram.client.impl

import rere.sasl.scram._
import rere.sasl.scram.cache.SaltedPasswordCache
import rere.sasl.scram.client.{AuthError, ClientFinalStep, ClientSecondStep}
import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.messages._
import rere.sasl.scram.rendering.SCRAMRenderer
import rere.sasl.util.{Base64, UTF8}

class ClientSecondStepImpl(
    clientFirstMessage: ClientFirstMessage,
    password: String,
    authMechanism: ScramAuthMechanism,
    cache: SaltedPasswordCache
  ) extends ClientSecondStep {

  override def firstMessage: ClientFirstMessage = clientFirstMessage

  override def process(serverFirstMessage: ServerFirstMessage): Either[AuthError, ClientFinalStep] = {

    if (serverFirstMessage.serverNonce.toString.startsWith(clientFirstMessage.bare.clientNonce.toString)) {

      val saltStr = serverFirstMessage.salt.toString()
      val i = serverFirstMessage.iterationCount

      val saltedPassword = cache.get(password, saltStr, i).getOrElse {
        val salted = authMechanism.hi(
          crypto.normalize(password),
          Base64.from(serverFirstMessage.salt),
          serverFirstMessage.iterationCount
        )

        cache.put(password, saltStr, i, salted)
        salted
      }

      val clientKey = authMechanism.hmac(saltedPassword, crypto.CLIENT_KEY)

      val storedKey = authMechanism.h(clientKey)

      val clientFinalMessageWithoutProof = ClientFinalMessageWithoutProof(
        Base64.to(UTF8.to(
          SCRAMRenderer.renderToString(clientFirstMessage.header)
        )),
        serverFirstMessage.serverNonce,
        Nil
      )

      val authMessage = InternalAuthMessage(
        clientFirstMessage.bare,
        serverFirstMessage,
        clientFinalMessageWithoutProof)

      val renderedAuthMessage = SCRAMRenderer.renderToString(authMessage)

      val clientSignature = authMechanism.hmac(storedKey, renderedAuthMessage)

      crypto.xor(clientKey, clientSignature) match {
        case Right(clientProof) =>
          val clientFinalMessage = ClientFinalMessage(
            clientFinalMessageWithoutProof,
            Base64.to(clientProof)
          )

          val serverKey = authMechanism.hmac(saltedPassword, crypto.SERVER_KEY)
          val serverSignature = authMechanism.hmac(serverKey, renderedAuthMessage)

          Right(new ClientFinalStepImpl(clientFinalMessage, Base64.to(serverSignature)))

        case Left(errorMessage) => Left(AuthError(errorMessage))
      }
    } else {
      Left(AuthError("Validation error: nonce is falsified"))
    }
  }
}
