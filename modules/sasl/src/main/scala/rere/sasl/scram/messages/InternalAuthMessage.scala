package rere.sasl.scram.messages

final case class InternalAuthMessage(
  clientFirstMessageBare: ClientFirstMessageBare,
  serverFirstMessage: ServerFirstMessage,
  clientFinalMessageWithoutProof: ClientFinalMessageWithoutProof)
