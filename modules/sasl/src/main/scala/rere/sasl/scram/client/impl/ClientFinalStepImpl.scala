package rere.sasl.scram.client.impl

import rere.sasl.scram.client.{AuthError, ClientFinalStep}
import rere.sasl.scram.messages.{ClientFinalMessage, ServerError, ServerFinalMessage}
import rere.sasl.util.Base64String

class ClientFinalStepImpl(
    clientFinalMessage: ClientFinalMessage,
    expectedServerSignature: Base64String
  ) extends ClientFinalStep {

  override def finalMessage: ClientFinalMessage = clientFinalMessage

  override def process(serverFinalMessage: ServerFinalMessage): Either[AuthError, String] = {
    serverFinalMessage.errorOrVerifier match {
      case Left(ServerError(serverErrorType)) =>
        Left(AuthError(s"Server error: ${serverErrorType.toString}"))

      case Right(verifier) =>
        if (isValidServerSignature(verifier)) {
          Right("OK")
        } else {
          Left(AuthError(s"Verification failed: ${verifier.str} != ${expectedServerSignature.str}"))
        }
    }
  }

  override def isValidServerSignature(serverSignature: Base64String): Boolean = {
    serverSignature.str == expectedServerSignature.str
  }
}
