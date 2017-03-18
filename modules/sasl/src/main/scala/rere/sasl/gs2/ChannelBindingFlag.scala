package rere.sasl.gs2

sealed trait ChannelBindingFlag
object ChannelBindingFlag {
  final case class SupportsAndUsed(channelBindingName: String) extends ChannelBindingFlag
  case object NotSupports extends ChannelBindingFlag
  case object SupportsButNotUsed extends ChannelBindingFlag
}
