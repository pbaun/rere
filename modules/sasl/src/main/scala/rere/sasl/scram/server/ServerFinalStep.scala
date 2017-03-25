package rere.sasl.scram.server

import rere.sasl.scram.messages.{ClientFinalMessage, ServerFinalMessage, ServerFirstMessage}

trait ServerFinalStep {
  def firstMessage: ServerFirstMessage
  def process(clientFinalMessage: ClientFinalMessage): ServerFinalMessage
}
