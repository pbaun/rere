package rere.sasl.scram.client.impl

import rere.sasl.scram.client.{AuthError, ClientFinalStep}
import rere.sasl.scram.messages.{ServerError, ServerFinalMessage}
import rere.sasl.util.Base64String

class ClientFinalStepImpl(private[client] val expectedServerSignature: Base64String) extends ClientFinalStep {
  override def process(serverFinalMessage: ServerFinalMessage): Either[AuthError, String] = {
    serverFinalMessage.errorOrVerifier match {
      case Left(ServerError(serverErrorType)) =>
        Left(AuthError(s"Server error: ${serverErrorType.toString}"))

      case Right(verifier) =>
        if (verifier == expectedServerSignature) {
          Right("OK")
        } else {
          Left(AuthError(s"Verification failed: ${verifier.str} != ${expectedServerSignature.str}"))
        }
    }
  }
}
