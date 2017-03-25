package rere.sasl.util

import org.scalatest.{FlatSpec, Matchers}

class StringRendererTest extends FlatSpec with Matchers {

  trait mocks {
    class UniqueClass
    implicit val uniqueClassRendering: Rendering[UniqueClass] = {
      new Rendering[UniqueClass] {
        override def render[R <: Renderer](r: R, obj: UniqueClass) = {
          r ~~ "unique class"
        }
      }
    }

    val renderer = new StringRenderer
  }

  behavior of "ByteStringRenderer"

  it should "be able to append String" in new mocks {
    renderer.~~("abc")
    renderer.get shouldBe "abc"
  }

  it should "be able to append object of any type that has instance of Rendering typeclass" in new mocks {
    renderer.~~(new UniqueClass)
    renderer.get shouldBe "unique class"
  }

  it should "be able to append Some[T]: Option[T] where T has instance of Rendering typeclass" in new mocks {
    renderer.~~(Some(new UniqueClass))
    renderer.get shouldBe "unique class"
  }

  it should "be able to append None: Option[T] where T has instance of Rendering typeclass" in new mocks {
    renderer.~~(None: Option[UniqueClass])
    renderer.get shouldBe ""
  }

}
