package rere.sasl.scram.messages

import rere.sasl.gs2

final case class ClientFirstMessage(
  header: gs2.Header,
  bare: ClientFirstMessageBare)
