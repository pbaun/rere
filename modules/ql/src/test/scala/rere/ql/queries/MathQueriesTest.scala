package rere.ql.queries

import org.scalatest.{Matchers, WordSpec}
import rere.ql.types._

class MathQueriesTest extends WordSpec with ReqlMatchers with Matchers {

  import rere.ql.queries.all.r
  import rere.ql.queries.math._
  import rere.ql.queries.values._

  "add operator" should {
    "be accessible on r and" should {
      "not allow to call it without arguments" in {
        "r.add()" shouldNot compile
        "r.expr(123).add()" should compile
      }

      "allow to add integers" in {
        r.add(123) shouldBe subtypeOf [ReqlInteger] and serializedTo("[24,[123]]")
        r.add(123, 234) shouldBe subtypeOf [ReqlInteger] and serializedTo("[24,[123,234]]")
        r.add(123, 234, 345) shouldBe subtypeOf [ReqlInteger] and serializedTo("[24,[123,234,345]]")
      }

      "allow to add floats" in {
        r.add(BigDecimal(123.123)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123]]")

        r.add(BigDecimal(123.123), BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234.234]]")

        r.add(BigDecimal(123.123), BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234.234,345.345]]")
      }

      "allow to add integers and floats" in {
        r.add(123, BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123,234.234]]")

        r.add(123, BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123,234.234,345.345]]")

        r.add(123, BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123,234.234,345]]")

        r.add(123, 234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123,234,345.345]]")

        r.add(BigDecimal(123.123), 234) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234]]")

        r.add(BigDecimal(123.123), 234, 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234,345]]")

        r.add(BigDecimal(123.123), 234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234,345.345]]")

        r.add(BigDecimal(123.123), BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234.234,345]]")
      }

      "allow to add strings" in {
        r.add("abc") shouldBe subtypeOf [ReqlString] and serializedTo("""[24,["abc"]]""")
        r.add("abc", "bcd") shouldBe subtypeOf [ReqlString] and serializedTo("""[24,["abc","bcd"]]""")
        r.add("abc", "bcd", "cde") shouldBe subtypeOf [ReqlString] and serializedTo("""[24,["abc","bcd","cde"]]""")
      }

      "allow to add elements from arrays" in {
        r.add(Seq(1, 2, 3): Seq[ReqlInteger]) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and serializedTo("""[24,[[2,[1,2,3]]]]""")

        r.add(
          Seq(1, 2, 3): Seq[ReqlInteger],
          Seq(4, 5, 6): Seq[ReqlInteger]
        ) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and
          serializedTo("""[24,[[2,[1,2,3]],[2,[4,5,6]]]]""")

        r.add(
          Seq(1, 2, 3): Seq[ReqlInteger],
          Seq(4, 5, 6): Seq[ReqlInteger],
          Seq(7, 8, 9): Seq[ReqlInteger]
        ) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and
          serializedTo("""[24,[[2,[1,2,3]],[2,[4,5,6]],[2,[7,8,9]]]]""")
      }

      "allow to add numbers to time" in {
        r.add(r.now()) shouldBe subtypeOf [ReqlTime] and serializedTo("[24,[[103,[]]]]")
        r.add(r.now(), 123) shouldBe subtypeOf [ReqlTime] and serializedTo("[24,[[103,[]],123]]")
        r.add(r.now(), 123, 234) shouldBe subtypeOf [ReqlTime] and serializedTo("[24,[[103,[]],123,234]]")
      }

      "support chaining" in {
        r.add(123).add(234) shouldBe subtypeOf [ReqlInteger] and serializedTo("[24,[[24,[123]],234]]")
        r.add(r.add(123)) shouldBe subtypeOf [ReqlInteger] and serializedTo("[24,[[24,[123]]]]")
        r.expr(123).add(r.add(234, 345)) shouldBe subtypeOf [ReqlInteger] and serializedTo("[24,[123,[24,[234,345]]]]")


        r.expr("abc").add(r.add("bcd", "cde")) shouldBe
          subtypeOf [ReqlString] and serializedTo("""[24,["abc",[24,["bcd","cde"]]]]""")
        r.add("abc").add("bcd") shouldBe subtypeOf [ReqlString] and serializedTo("""[24,[[24,["abc"]],"bcd"]]""")
        r.add(r.add("abc")) shouldBe subtypeOf [ReqlString] and serializedTo("""[24,[[24,["abc"]]]]""")


        r.add(Seq(1, 2, 3): Seq[ReqlInteger])
          .add(Seq(4, 5, 6): Seq[ReqlInteger]) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and
          serializedTo("""[24,[[24,[[2,[1,2,3]]]],[2,[4,5,6]]]]""")

        r.add(r.add(Seq(1, 2, 3): Seq[ReqlInteger])) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and serializedTo("""[24,[[24,[[2,[1,2,3]]]]]]""")

        r.expr(Seq(1, 2, 3): Seq[ReqlInteger])
          .add(r.add(
            Seq(4, 5, 6): Seq[ReqlInteger],
            Seq(7, 8, 9): Seq[ReqlInteger])
          ) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and
          serializedTo("""[24,[[2,[1,2,3]],[24,[[2,[4,5,6]],[2,[7,8,9]]]]]]""")


        "r.add(r.now()).add(r.now())" shouldNot compile
        r.add(r.now()).add(123) shouldBe subtypeOf [ReqlTime] and serializedTo("""[24,[[24,[[103,[]]]],123]]""")
        r.add(r.add(r.now())) shouldBe subtypeOf [ReqlTime] and serializedTo("""[24,[[24,[[103,[]]]]]]""")
        r.now().add(r.add(123, 234)) shouldBe subtypeOf [ReqlTime] and serializedTo("""[24,[[103,[]],[24,[123,234]]]]""")
      }
    }

    "be accessible on ReqlInteger and" should {
      "allow to add integers" in {
        r.expr(123).add() shouldBe subtypeOf [ReqlInteger] and serializedTo("[24,[123]]")
        r.expr(123).add(234) shouldBe subtypeOf [ReqlInteger] and serializedTo("[24,[123,234]]")
        r.expr(123).add(234, 345) shouldBe subtypeOf [ReqlInteger] and serializedTo("[24,[123,234,345]]")
      }

      "allow to add floats" in {
        r.expr(123).add(BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123,234.234]]")

        r.expr(123).add(BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf[ReqlFloat] and serializedTo("[24,[123,234.234,345.345]]")
      }

      "allow to add integers and floats" in {
        r.expr(123).add(BigDecimal(234.234), 345) shouldBe
          subtypeOf[ReqlFloat] and serializedTo("[24,[123,234.234,345]]")

        r.expr(123).add(234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123,234,345.345]]")
      }
    }

    "be accessible on ReqlFloat and" should {
      "allow to add floats" in {
        r.expr(BigDecimal(123.123)).add() shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123]]")

        r.expr(BigDecimal(123.123)).add(BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234.234]]")

        r.expr(BigDecimal(123.123)).add(BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234.234,345.345]]")
      }

      "allow to add integers" in {
        r.expr(BigDecimal(123.123)).add(234) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234]]")

        r.expr(BigDecimal(123.123)).add(234, 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234,345]]")
      }

      "allow to add floats and integers" in {
        r.expr(BigDecimal(123.123)).add(234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234,345.345]]")

        r.expr(BigDecimal(123.123)).add(BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123.123,234.234,345]]")
      }
    }

    "be accessible on ReqlString and" should {
      "allow to add strings" in {
        r.expr("abc").add() shouldBe subtypeOf [ReqlString] and serializedTo("""[24,["abc"]]""")
        r.expr("abc").add("bcd") shouldBe subtypeOf [ReqlString] and serializedTo("""[24,["abc","bcd"]]""")
        r.expr("abc").add("bcd", "cde") shouldBe subtypeOf [ReqlString] and serializedTo("""[24,["abc","bcd","cde"]]""")
      }
    }

    "be accessible on ReqlArray and" should {
      "allow to add elements from arrays" in {
        r.expr(Seq(1, 2, 3): Seq[ReqlInteger]).add() shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and serializedTo("""[24,[[2,[1,2,3]]]]""")

        r.expr(Seq(1, 2, 3): Seq[ReqlInteger])
          .add(Seq(4, 5, 6): Seq[ReqlInteger]) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and serializedTo("""[24,[[2,[1,2,3]],[2,[4,5,6]]]]""")

        r.expr(Seq(1, 2, 3): Seq[ReqlInteger])
          .add(
            Seq(4, 5, 6): Seq[ReqlInteger],
            Seq(7, 8, 9): Seq[ReqlInteger]
          ) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and
          serializedTo("""[24,[[2,[1,2,3]],[2,[4,5,6]],[2,[7,8,9]]]]""")
      }
    }

    "be accessible on ReqlTime and" should {
      "allow to add numbers to time" in {
        r.now().add() shouldBe subtypeOf [ReqlTime] and serializedTo("[24,[[103,[]]]]")
        r.now().add(123) shouldBe subtypeOf [ReqlTime] and serializedTo("[24,[[103,[]],123]]")
        r.now().add(123, 234) shouldBe subtypeOf [ReqlTime] and serializedTo("[24,[[103,[]],123,234]]")
      }
    }
  }
}
