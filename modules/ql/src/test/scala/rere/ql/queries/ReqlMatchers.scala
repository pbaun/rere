package rere.ql.queries

import org.scalatest.Matchers._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Assertion, WordSpec}
import rere.ql.rasterization
import rere.ql.types.{ReqlExpr, ReqlInteger, ReqlString}

trait ReqlMatchers {

  // expr shouldBe ...
  implicit class ReqlExprWrapper[Expr <: ReqlExpr](expr: Expr) {
    def shouldBe[SuperExpr <: ReqlExpr](
      result: ResultOfSuperTypeCapturing[SuperExpr])(
      implicit ev: Expr <:< SuperExpr
    ): TypeCheckedExpr = {
      new TypeCheckedExpr(expr)
    }

    def shouldBe(result: ResultOfSerializedFormCapturing): Assertion = {
      result.checkSerializedForm(expr)
    }
  }

  // ... subtypeOf [SuperExpr] ...
  def subtypeOf[SuperExpr <: ReqlExpr]: ResultOfSuperTypeCapturing[SuperExpr] = {
    new ResultOfSuperTypeCapturing
  }
  class ResultOfSuperTypeCapturing[SuperExpr <: ReqlExpr]

  // ... and ...
  class TypeCheckedExpr(expr: ReqlExpr) extends Assertion {
    def and(result: ResultOfSerializedFormCapturing): Assertion = {
      result.checkSerializedForm(expr)
    }
  }

  // ... serializedTo("...")
  def serializedTo(expectedSerializedForm: String): ResultOfSerializedFormCapturing = {
    new ResultOfSerializedFormCapturing(expectedSerializedForm)
  }
  class ResultOfSerializedFormCapturing(expectedSerializedForm: String) {
    def checkSerializedForm(expr: ReqlExpr): Assertion = {
      import rasterization.building._
      expr.build().utf8String shouldBe expectedSerializedForm
    }
  }
}

class ReqlMatchersTest extends WordSpec with ReqlMatchers {

  import rere.ql.queries.all._

  "type matcher" should {
    "allow strict type match" in {
      r.expr(123) shouldBe subtypeOf [ReqlInteger]
    }

    "allow subtype match" in {
      r.uuid() shouldBe subtypeOf [ReqlString]
    }

    "not allow type mismatch in compile time" in {
      """r.uuid() shouldBe subtypeOf [ReqlInteger]""" shouldNot compile
    }
  }

  "serialized form matcher" should {
    "allow strict match" in {
      r.uuid() shouldBe serializedTo("[169,[]]")
    }

    "not allow serialized form mismatch in runtime" in {
      the [TestFailedException] thrownBy {
        r.uuid() shouldBe serializedTo("[169,[1]]")
      } should have message """"[169,[[]]]" was not equal to "[169,[[1]]]""""
    }
  }

  "type matcher and serialized form matcher together" should {
    "allow strict type and serialized form match" in {
      r.expr(123) shouldBe subtypeOf [ReqlInteger] and serializedTo("""123""")
    }

    "allow subtype match and strict serialized form match" in {
      r.uuid() shouldBe subtypeOf [ReqlString] and serializedTo("[169,[]]")
    }

    "not allow type mismatch in compile time" in {
      """r.uuid() shouldBe subtypeOf [ReqlInteger] and serializedTo("[169,[]]")""" shouldNot compile
    }

    "not allow serialized form mismatch in runtime" in {
      the [TestFailedException] thrownBy {
        r.uuid() shouldBe subtypeOf [ReqlString] and serializedTo("[169,[1]]")
      } should have message """"[169,[[]]]" was not equal to "[169,[[1]]]""""
    }
  }
}
