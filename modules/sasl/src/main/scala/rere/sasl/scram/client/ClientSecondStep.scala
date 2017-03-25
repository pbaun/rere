package rere.sasl.scram.client

import rere.sasl.scram.messages.{ClientFirstMessage, ServerFirstMessage}

trait ClientSecondStep {
  def firstMessage: ClientFirstMessage
  def process(serverFirstMessage: ServerFirstMessage): Either[AuthError, ClientFinalStep]
}
