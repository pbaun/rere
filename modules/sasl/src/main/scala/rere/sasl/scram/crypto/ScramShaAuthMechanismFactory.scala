package rere.sasl.scram.crypto

trait ScramShaAuthMechanismFactory {
  def getMechanism(errorReporter: ErrorReporter): ScramShaAuthMechanism
}
