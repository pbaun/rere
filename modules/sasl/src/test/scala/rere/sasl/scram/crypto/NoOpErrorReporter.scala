package rere.sasl.scram.crypto

class NoOpErrorReporter extends ErrorReporter {
  override def onError(throwable: Throwable) = ()
}
