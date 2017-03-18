package rere.sasl.scram.client.impl

import akka.util.ByteString
import rere.sasl.gs2
import rere.sasl.gs2.ChannelBindingFlag
import rere.sasl.scram.client.{ClientFirstStep, ClientSecondStep, SaltedPasswordCache}
import rere.sasl.scram.crypto
import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.scram.messages.{AttrVal, ClientFirstMessage, ClientFirstMessageBare}
import rere.sasl.scram.rendering._
import rere.sasl.util.{EscapedString, Renderer}

class ClientFirstStepImpl(
    authMechanism: ScramAuthMechanism,
    entropySource: EntropySource,
    cache: SaltedPasswordCache
  ) extends ClientFirstStep {

  override def auth(
    username: String,
    password: String,
    binding: ChannelBindingFlag,
    authId: Option[String],
    extensions: Seq[AttrVal]
  ): (ByteString, ClientSecondStep) = {

    val safeUsername = EscapedString.to(crypto.normalize(username))
    val nonce = entropySource.nonce(authMechanism.DEFAULT_NONCE_LENGTH)
    val bare = ClientFirstMessageBare(None, safeUsername, nonce, extensions)

    val clientFirstMessage = ClientFirstMessage(
      gs2.Header(binding, authId.map(a => EscapedString.to(crypto.normalize(a)))),
      bare
    )

    (
      Renderer.renderToByteString(clientFirstMessage),
      new ClientSecondStepImpl(clientFirstMessage, password, authMechanism, cache)
    )
  }
}
