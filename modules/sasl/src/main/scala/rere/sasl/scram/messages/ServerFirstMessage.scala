package rere.sasl.scram.messages

import rere.sasl.util.{Base64String, PrintableAndSafe}

final case class ServerFirstMessage(
  reserved: Option[AttrVal],
  serverNonce: PrintableAndSafe,
  salt: Base64String,
  iterationCount: Int,
  extensions: Seq[AttrVal])
