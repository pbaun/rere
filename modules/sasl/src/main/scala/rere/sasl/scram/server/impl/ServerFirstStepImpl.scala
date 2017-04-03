package rere.sasl.scram.server.impl

import rere.sasl.scram.crypto
import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.scram.messages.{ClientFirstMessage, ServerFirstMessage}
import rere.sasl.scram.server.{ServerFinalStep, ServerFirstStep}
import rere.sasl.scram.storage.SaltedPasswordStorage
import rere.sasl.util.{Base64, EscapedString, PrintableString}

class ServerFirstStepImpl(
    authMechanism: ScramAuthMechanism,
    entropySource: EntropySource,
    storage: SaltedPasswordStorage
  ) extends ServerFirstStep {

  override def process(clientFirstMessage: ClientFirstMessage): ServerFinalStep = {

    val username = crypto.normalize(EscapedString.from(clientFirstMessage.bare.username))
    val authData = storage.getAuthData(username)
    val nonce = entropySource.nonce(authMechanism.DEFAULT_NONCE_LENGTH)

    val serverNonce = new PrintableString(clientFirstMessage.bare.clientNonce.toString + nonce.toString)
    val serverFirstMessage = ServerFirstMessage(None, serverNonce, Base64.to(authData.salt), authData.i, Nil)

    new ServerFinalStepImpl(clientFirstMessage, serverFirstMessage, authData, serverNonce, authMechanism)
  }
}
