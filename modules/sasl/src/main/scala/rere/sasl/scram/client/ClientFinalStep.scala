package rere.sasl.scram.client

import rere.sasl.scram.messages.{ClientFinalMessage, ServerFinalMessage}
import rere.sasl.util.Base64String

trait ClientFinalStep {
  def finalMessage: ClientFinalMessage
  def process(serverFinalMessage: ServerFinalMessage): Either[AuthError, String]
  def isValidServerSignature(serverSignature: Base64String): Boolean
}
