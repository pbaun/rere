package rere.sasl.scram.crypto

trait ErrorReporter {
  def onError(throwable: Throwable): Unit
}
