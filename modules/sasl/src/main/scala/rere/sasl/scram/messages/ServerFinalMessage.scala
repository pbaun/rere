package rere.sasl.scram.messages

import rere.sasl.util.Base64String

case class ServerFinalMessage(
  errorOrVerifier: Either[ServerError, Base64String],
  extensions: Seq[AttrVal])
