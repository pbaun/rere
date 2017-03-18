package rere.sasl.scram.crypto.sha1

import rere.sasl.scram.crypto.{ErrorReporter, ScramShaAuthMechanism, ScramShaAuthMechanismFactory}

object ScramSha1AuthMechanismFactory extends ScramShaAuthMechanismFactory {
  override def getMechanism(errorReporter: ErrorReporter): ScramShaAuthMechanism = {
    new ScramSha1AuthMechanism(errorReporter)
  }
}
