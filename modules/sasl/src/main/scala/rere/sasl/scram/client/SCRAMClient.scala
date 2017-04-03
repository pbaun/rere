package rere.sasl.scram.client

import rere.sasl.scram.cache.SaltedPasswordCache
import rere.sasl.scram.crypto.ScramAuthMechanism
import rere.sasl.scram.crypto.entropy.EntropySource

object SCRAMClient {
  def apply(
    authMechanism: ScramAuthMechanism,
    entropySource: EntropySource,
    cache: SaltedPasswordCache
  ): ClientFirstStep = {
    new impl.ClientFirstStepImpl(authMechanism, entropySource, cache)
  }
}
