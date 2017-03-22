package rere.sasl.scram.crypto.sha256

import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import rere.sasl.scram.crypto.ErrorReporter

class ScramSha256AuthMechanismTest extends FlatSpec with MockFactory {

  behavior of "ScramSha256AuthMechanism"

  it should "report about errors to error reporter" in {
    val reporter = stub[ErrorReporter]
    val mechanism = ScramSha256AuthMechanismFactory.getMechanism(reporter)

    val ex = intercept[NullPointerException] {
      mechanism.h(null)
    }

    reporter.onError _ verify ex
  }

}
