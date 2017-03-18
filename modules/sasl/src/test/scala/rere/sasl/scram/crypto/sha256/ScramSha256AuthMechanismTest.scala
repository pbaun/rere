package rere.sasl.scram.crypto.sha256

import org.mockito.Mockito.{verify, verifyNoMoreInteractions}
import org.scalatest.FlatSpec
import org.scalatest.mockito.MockitoSugar
import rere.sasl.scram.crypto.ErrorReporter

class ScramSha256AuthMechanismTest extends FlatSpec with MockitoSugar {

  behavior of "ScramSha256AuthMechanism"

  it should "report about errors to error reporter" in {
    val reporter = mock[ErrorReporter]
    val mechanism = ScramSha256AuthMechanismFactory.getMechanism(reporter)

    val ex = intercept[NullPointerException] {
      mechanism.h(null)
    }

    verify(reporter).onError(ex)
    verifyNoMoreInteractions(reporter)
  }

}
