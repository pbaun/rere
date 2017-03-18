package rere.sasl.scram.server.impl

import rere.sasl.gs2.ChannelBindingFlag.NotSupports
import rere.sasl.scram._
import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.messages._
import rere.sasl.scram.rendering._
import rere.sasl.scram.server.{AuthData, ServerFinalStep}
import rere.sasl.util.{Base64, PrintableAndSafe, Renderer, UTF8}

class ServerFinalStepImpl(
    clientFirstMessage: ClientFirstMessage,
    serverFirstMessage: ServerFirstMessage,
    authData: AuthData,
    serverNonce: PrintableAndSafe,
    authMechanism: ScramAuthMechanism
  ) extends ServerFinalStep {

  def process(clientFinalMessage: ClientFinalMessage): ServerFinalMessage = {

    if (authData.isReal) {
      if (clientFinalMessage.bare.nonce.toString == serverNonce.toString) {
        val renderedBinding = Renderer.renderToString(clientFirstMessage.header)
        val codedBinding = Base64.to(UTF8.to(renderedBinding))

        if (clientFinalMessage.bare.channelBinding.toString == codedBinding.toString) {
          if (clientFirstMessage.header.channelBinding == NotSupports) {

            val proof = Base64.from(clientFinalMessage.proof)

            if (proof.length == authMechanism.CLIENT_PROOF_LENGTH) {

              val authMessage = InternalAuthMessage(
                clientFirstMessage.bare,
                serverFirstMessage,
                clientFinalMessage.bare)

              val renderedAuthMessage = Renderer.renderToString(authMessage)
              val clientSignature = authMechanism.hmac(authData.storedKey, renderedAuthMessage)

              crypto.xor(authData.clientKey, clientSignature) match {
                case Right(clientProof) =>
                  if (proof.sameElements(clientProof)) {
                    val serverKey = authMechanism.hmac(authData.saltedPassword, crypto.SERVER_KEY)
                    val serverSignature = authMechanism.hmac(serverKey, renderedAuthMessage)

                    ServerFinalMessage(Right(Base64.to(serverSignature)), Nil)
                  } else {
                    error(ServerErrorType.InvalidProof)
                  }

                case Left(_) =>
                  error(ServerErrorType.InvalidProof)
              }
            } else {
              error(ServerErrorType.InvalidProof)
            }
          } else {
            error(ServerErrorType.ServerDoesSupportChannelBinding)
          }
        } else {
          error(ServerErrorType.ChannelBindingsDontMatch)
        }
      } else {
        error(ServerErrorType.InvalidEncoding)
      }
    } else {
      error(ServerErrorType.UnknownUser)
    }
  }

  private def error(errorType: ServerErrorType): ServerFinalMessage = {
    ServerFinalMessage(Left(ServerError(errorType)), Nil)
  }
}
