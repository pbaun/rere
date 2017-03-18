package rere.sasl.scram.server

import rere.sasl.scram.messages.{ClientFinalMessage, ServerFinalMessage}

trait ServerFinalStep {
  def process(clientFinalMessage: ClientFinalMessage): ServerFinalMessage
}
