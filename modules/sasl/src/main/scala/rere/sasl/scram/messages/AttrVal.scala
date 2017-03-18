package rere.sasl.scram.messages

import rere.sasl.util.SafeString

final case class AttrVal(attribute: Char, value: SafeString)
