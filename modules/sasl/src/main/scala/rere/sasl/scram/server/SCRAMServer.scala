package rere.sasl.scram.server

import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.scram.storage.SaltedPasswordStorage

object SCRAMServer {
  def apply(
    authMechanism: ScramAuthMechanism,
    entropySource: EntropySource,
    storage: SaltedPasswordStorage
  ): ServerFirstStep = {
    new impl.ServerFirstStepImpl(authMechanism, entropySource, storage)
  }
}
