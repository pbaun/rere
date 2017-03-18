package rere.sasl.scram.messages

import rere.sasl.util.SafeString

sealed trait ServerErrorType
object ServerErrorType {
  final case object InvalidEncoding extends ServerErrorType
  final case object ExtensionsNotSupported extends ServerErrorType
  final case object InvalidProof extends ServerErrorType
  final case object ChannelBindingsDontMatch extends ServerErrorType
  final case object ServerDoesSupportChannelBinding extends ServerErrorType
  final case object ChannelBindingNotSupported extends ServerErrorType
  final case object UnsupportedChannelBindingType extends ServerErrorType
  final case object UnknownUser extends ServerErrorType
  final case object InvalidUsernameEncoding extends ServerErrorType
  final case object NoResources extends ServerErrorType
  final case object OtherError extends ServerErrorType
  final case class UnrecognizedError(message: SafeString) extends ServerErrorType
}
