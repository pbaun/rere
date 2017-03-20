package rere.sasl.util

import akka.util.ByteString
import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}

class RendererTest extends FlatSpec with MockitoSugar with Matchers {

  trait mocks {
    val rendererMock = mock[Renderer]

    class UniqueClass
    implicit val uniqueClassRendering: Rendering[UniqueClass] = {
      new Rendering[UniqueClass] {
        override def render[R <: Renderer](r: R, obj: UniqueClass) = {
          r ~~ "unique class"
        }
      }
    }
  }

  behavior of "Renderer companion"

  it should "render object using provided renderer and implicit Rendering" in new mocks {
    when(rendererMock.~~(any[String]())).thenReturn(rendererMock: rendererMock.type)

    Renderer.render(rendererMock, new UniqueClass) shouldBe rendererMock

    verify(rendererMock).~~("unique class")
    verifyNoMoreInteractions(rendererMock)
  }

  it should "render object to String using implicit Rendering" in new mocks {
    Renderer.renderToString(new UniqueClass) shouldBe "unique class"
  }

  it should "render object to ByteString using implicit Rendering" in new mocks {
    Renderer.renderToByteString(new UniqueClass) shouldBe ByteString("unique class")
  }

}
