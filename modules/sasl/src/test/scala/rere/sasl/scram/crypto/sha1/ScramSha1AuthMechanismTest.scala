package rere.sasl.scram.crypto.sha1

import org.mockito.Mockito._
import org.scalatest.FlatSpec
import org.scalatest.mockito.MockitoSugar
import rere.sasl.scram.crypto.ErrorReporter

class ScramSha1AuthMechanismTest extends FlatSpec with MockitoSugar {

  behavior of "ScramSha1AuthMechanism"

  it should "report about errors to error reporter" in {
    val reporter = mock[ErrorReporter]
    val mechanism = ScramSha1AuthMechanismFactory.getMechanism(reporter)

    val ex = intercept[NullPointerException] {
      mechanism.h(null)
    }

    verify(reporter).onError(ex)
    verifyNoMoreInteractions(reporter)
  }

}
