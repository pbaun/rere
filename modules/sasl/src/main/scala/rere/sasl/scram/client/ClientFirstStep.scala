package rere.sasl.scram.client

import akka.util.ByteString
import rere.sasl.gs2.ChannelBindingFlag
import rere.sasl.scram.messages.AttrVal

trait ClientFirstStep {
  def auth(
    username: String,
    password: String,
    binding: ChannelBindingFlag,
    authId: Option[String],
    extensions: Seq[AttrVal]
  ): (ByteString, ClientSecondStep)
}
