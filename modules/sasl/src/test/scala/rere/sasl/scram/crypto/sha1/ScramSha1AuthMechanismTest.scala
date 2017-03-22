package rere.sasl.scram.crypto.sha1

import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import rere.sasl.scram.crypto.ErrorReporter

class ScramSha1AuthMechanismTest extends FlatSpec with MockFactory {

  behavior of "ScramSha1AuthMechanism"

  it should "report about errors to error reporter" in {
    val reporter = stub[ErrorReporter]
    val mechanism = ScramSha1AuthMechanismFactory.getMechanism(reporter)

    val ex = intercept[NullPointerException] {
      mechanism.h(null)
    }

    reporter.onError _ verify ex
  }

}
