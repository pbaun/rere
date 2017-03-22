package rere.sasl.scram.client

import rere.sasl.scram.messages.ServerFinalMessage
import rere.sasl.util.Base64String

trait ClientFinalStep {
  def process(serverFinalMessage: ServerFinalMessage): Either[AuthError, String]
  def isValidServerSignature(serverSignature: Base64String): Boolean
}
