package rere.sasl.scram.messages

import rere.sasl.util.{EscapedString, PrintableAndSafe}

final case class ClientFirstMessageBare(
  reserved: Option[AttrVal],
  username: EscapedString,
  clientNonce: PrintableAndSafe,
  extensions: Seq[AttrVal])
