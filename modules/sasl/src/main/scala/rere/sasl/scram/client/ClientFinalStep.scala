package rere.sasl.scram.client

import rere.sasl.scram.messages.ServerFinalMessage
import rere.sasl.util.Base64String

trait ClientFinalStep {
  private[client] def expectedServerSignature: Base64String
  def process(serverFinalMessage: ServerFinalMessage): Either[AuthError, String]
}
