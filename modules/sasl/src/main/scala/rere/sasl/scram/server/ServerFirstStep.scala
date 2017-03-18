package rere.sasl.scram.server

import akka.util.ByteString
import rere.sasl.scram.messages.ClientFirstMessage

trait ServerFirstStep {
  def process(clientFirstMessage: ClientFirstMessage): (ByteString, ServerFinalStep)
}
