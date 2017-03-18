package rere.sasl.util

import java.nio.charset.StandardCharsets

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}

class ByteStringRendererTest extends FlatSpec with Matchers {

  trait mocks {
    class UniqueClass
    implicit val uniqueClassRendering: Rendering[UniqueClass] = {
      new Rendering[UniqueClass] {
        override def render[R <: Renderer](r: R, obj: UniqueClass) = {
          r ~~ "unique class"
        }
      }
    }

    val renderer = new ByteStringRenderer(StandardCharsets.UTF_8)
  }

  behavior of "ByteStringRenderer"

  it should "be able to append Array[Byte]" in new mocks {
    renderer.~~(Array(97.toByte, 98.toByte, 99.toByte))
    renderer.get shouldBe ByteString("abc")
  }

  it should "be able to append String" in new mocks {
    renderer.~~("abc")
    renderer.get shouldBe ByteString("abc")
  }

  it should "be able to append object of any type that has instance of Rendering typeclass" in new mocks {
    renderer.~~(new UniqueClass)
    renderer.get shouldBe ByteString("unique class")
  }

  it should "be able to append Some[T]: Option[T] where T has instance of Rendering typeclass" in new mocks {
    renderer.~~(Some(new UniqueClass))
    renderer.get shouldBe ByteString("unique class")
  }

  it should "be able to append None: Option[T] where T has instance of Rendering typeclass" in new mocks {
    renderer.~~(None: Option[UniqueClass])
    renderer.get shouldBe ByteString.empty
  }

}
