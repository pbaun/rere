package rere.sasl.scram.messages

import rere.sasl.util.Base64String

final case class ClientFinalMessage(
  bare: ClientFinalMessageWithoutProof,
  proof: Base64String)
