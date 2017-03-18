package rere.sasl.scram.client

import akka.util.ByteString
import rere.sasl.scram.messages.ServerFirstMessage

trait ClientSecondStep {
  def process(serverFirstMessage: ServerFirstMessage): Either[AuthError, (ByteString, ClientFinalStep)]
}
