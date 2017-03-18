package rere.sasl.scram.crypto.sha256

import rere.sasl.scram.crypto.{ErrorReporter, ScramShaAuthMechanism, ScramShaAuthMechanismFactory}

object ScramSha256AuthMechanismFactory extends ScramShaAuthMechanismFactory {
  override def getMechanism(errorReporter: ErrorReporter): ScramShaAuthMechanism = {
    new ScramSha256AuthMechanism(errorReporter)
  }
}
