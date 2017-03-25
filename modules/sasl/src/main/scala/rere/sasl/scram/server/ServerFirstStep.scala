package rere.sasl.scram.server

import rere.sasl.scram.messages.ClientFirstMessage

trait ServerFirstStep {
  def process(clientFirstMessage: ClientFirstMessage): ServerFinalStep
}
