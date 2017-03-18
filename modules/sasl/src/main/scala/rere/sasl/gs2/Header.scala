package rere.sasl.gs2

import rere.sasl.util.EscapedString

final case class Header(channelBinding: ChannelBindingFlag, authId: Option[EscapedString])