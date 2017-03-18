package rere.sasl.scram.messages

import rere.sasl.util.{Base64String, PrintableAndSafe}

final case class ClientFinalMessageWithoutProof(
  channelBinding: Base64String,
  nonce: PrintableAndSafe,
  extensions: Seq[AttrVal])
