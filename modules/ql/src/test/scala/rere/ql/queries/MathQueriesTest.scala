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
        r.expr(123).add(r.add(234, 345)) shouldBe
          subtypeOf [ReqlInteger] and serializedTo("[24,[123,[24,[234,345]]]]")


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
        r.now().add(r.add(123, 234)) shouldBe
          subtypeOf [ReqlTime] and serializedTo("""[24,[[103,[]],[24,[123,234]]]]""")
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
          subtypeOf [ReqlFloat] and serializedTo("[24,[123,234.234,345.345]]")
      }

      "allow to add integers and floats" in {
        r.expr(123).add(BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[24,[123,234.234,345]]")

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
        r.expr("abc").add("bcd", "cde") shouldBe
          subtypeOf [ReqlString] and serializedTo("""[24,["abc","bcd","cde"]]""")
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

  "sub operator" should {
    "be accessible on r and" should {
      "not allow to call it without arguments" in {
        "r.sub()" shouldNot compile
        "r.expr(123).sub()" should compile
      }

      "allow to subtract integers" in {
        r.sub(123) shouldBe subtypeOf [ReqlInteger] and serializedTo("[25,[123]]")
        r.sub(123, 234) shouldBe subtypeOf [ReqlInteger] and serializedTo("[25,[123,234]]")
        r.sub(123, 234, 345) shouldBe subtypeOf [ReqlInteger] and serializedTo("[25,[123,234,345]]")
      }

      "allow to subtract floats" in {
        r.sub(BigDecimal(123.123)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123]]")

        r.sub(BigDecimal(123.123), BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234.234]]")

        r.sub(BigDecimal(123.123), BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234.234,345.345]]")
      }

      "allow to subtract integers and floats" in {
        r.sub(123, BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123,234.234]]")

        r.sub(123, BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123,234.234,345.345]]")

        r.sub(123, BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123,234.234,345]]")

        r.sub(123, 234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123,234,345.345]]")

        r.sub(BigDecimal(123.123), 234) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234]]")

        r.sub(BigDecimal(123.123), 234, 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234,345]]")

        r.sub(BigDecimal(123.123), 234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234,345.345]]")

        r.sub(BigDecimal(123.123), BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234.234,345]]")
      }

      "allow to subtract numbers from time" in {
        r.sub(r.now()) shouldBe subtypeOf [ReqlTime] and serializedTo("[25,[[103,[]]]]")
        r.sub(r.now(), 1) shouldBe subtypeOf [ReqlTime] and serializedTo("[25,[[103,[]],1]]")
        r.sub(r.now(), 1, 2) shouldBe subtypeOf [ReqlTime] and serializedTo("[25,[[103,[]],1,2]]")
      }

      "allow to subtract time from time" in {
        r.sub(r.now(), r.now()) shouldBe subtypeOf [ReqlFloat] and serializedTo("[25,[[103,[]],[103,[]]]]")
        "r.sub(r.now(), r.now(), r.now())" shouldNot compile
      }

      "support chaining" in {
        r.sub(123).sub(234) shouldBe subtypeOf [ReqlInteger] and serializedTo("[25,[[25,[123]],234]]")
        r.sub(r.sub(123)) shouldBe subtypeOf [ReqlInteger] and serializedTo("[25,[[25,[123]]]]")
        r.expr(123).sub(r.sub(234, 345)) shouldBe
          subtypeOf [ReqlInteger] and serializedTo("[25,[123,[25,[234,345]]]]")
      }
    }

    "be accessible on ReqlInteger and" should {
      "allow to subtract integers" in {
        r.expr(123).sub() shouldBe subtypeOf [ReqlInteger] and serializedTo("[25,[123]]")
        r.expr(123).sub(234) shouldBe subtypeOf [ReqlInteger] and serializedTo("[25,[123,234]]")
        r.expr(123).sub(234, 345) shouldBe subtypeOf [ReqlInteger] and serializedTo("[25,[123,234,345]]")
      }

      "allow to subtract floats" in {
        r.expr(123).sub(BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123,234.234]]")

        r.expr(123).sub(BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123,234.234,345.345]]")
      }

      "allow to subtract integers and floats" in {
        r.expr(123).sub(BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123,234.234,345]]")

        r.expr(123).sub(234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123,234,345.345]]")
      }
    }

    "be accessible on ReqlFloat and" should {
      "allow to subtract floats" in {
        r.expr(BigDecimal(123.123)).sub() shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123]]")

        r.expr(BigDecimal(123.123)).sub(BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234.234]]")

        r.expr(BigDecimal(123.123)).sub(BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234.234,345.345]]")
      }

      "allow to subtract integers" in {
        r.expr(BigDecimal(123.123)).sub(234) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234]]")

        r.expr(BigDecimal(123.123)).sub(234, 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234,345]]")
      }

      "allow to subtract floats and integers" in {
        r.expr(BigDecimal(123.123)).sub(234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234,345.345]]")

        r.expr(BigDecimal(123.123)).sub(BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[123.123,234.234,345]]")
      }
    }

    "be accessible on ReqlTime and" should {
      "allow to subtract numbers from time" in {
        r.now().sub() shouldBe subtypeOf [ReqlTime] and serializedTo("[25,[[103,[]]]]")
        r.now().sub(1) shouldBe subtypeOf [ReqlTime] and serializedTo("[25,[[103,[]],1]]")
        r.now().sub(1, 2) shouldBe subtypeOf [ReqlTime] and serializedTo("[25,[[103,[]],1,2]]")
      }

      "allow to subtract time from time" in {
        r.now().sub(r.now()) shouldBe subtypeOf [ReqlFloat] and serializedTo("[25,[[103,[]],[103,[]]]]")
      }

      "not allow to subtract time and numbers from time" in {
        "r.now().sub(r.now(), 1)" shouldNot compile
        r.now().sub(r.now()).sub(1, 2) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[25,[[25,[[103,[]],[103,[]]]],1,2]]")
      }
    }
  }

  "mul operator" should {
    "be accessible on ReqlInteger and" should {
      "allow to multiply integer by integers" in {
        r.expr(123).mul() shouldBe subtypeOf [ReqlInteger] and serializedTo("[26,[123]]")
        r.expr(123).mul(234) shouldBe subtypeOf [ReqlInteger] and serializedTo("[26,[123,234]]")
        r.expr(123).mul(234, 345) shouldBe subtypeOf [ReqlInteger] and serializedTo("[26,[123,234,345]]")
      }

      "allow to multiply integer by floats" in {
        r.expr(123).mul(BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123,234.234]]")

        r.expr(123).mul(BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123,234.234,345.345]]")

        r.expr(123).mul(BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123,234.234,345]]")

        r.expr(123).mul(234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123,234,345.345]]")
      }
    }

    "be accessible on ReqlFloat and" should {
      "allow to multiply float by floats" in {
        r.expr(BigDecimal(123.123)).mul() shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123.123]]")

        r.expr(BigDecimal(123.123)).mul(BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123.123,234.234]]")

        r.expr(BigDecimal(123.123)).mul(BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123.123,234.234,345.345]]")
      }

      "allow to multiply float by integers" in {
        r.expr(BigDecimal(123.123)).mul(234) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123.123,234]]")

        r.expr(BigDecimal(123.123)).mul(234, 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123.123,234,345]]")

        r.expr(BigDecimal(123.123)).mul(234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123.123,234,345.345]]")

        r.expr(BigDecimal(123.123)).mul(BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[26,[123.123,234.234,345]]")
      }
    }

    "be accessible on ReqlArray and" should {
      "allow to multiply array by integers" in {
        r.expr(Seq(r.expr(123), r.expr(234), r.expr(345))).mul() shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and serializedTo("[26,[[2,[123,234,345]]]]")

        r.expr(Seq(r.expr(123), r.expr(234), r.expr(345))).mul(6) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and serializedTo("[26,[[2,[123,234,345]],6]]")

        r.expr(Seq(r.expr(123), r.expr(234), r.expr(345))).mul(6, 7) shouldBe
          subtypeOf [ReqlArray[ReqlInteger]] and serializedTo("[26,[[2,[123,234,345]],6,7]]")

        "r.expr(Seq(r.expr(123), r.expr(234), r.expr(345))).mul(BigDecimal(2.5))" shouldNot compile
      }
    }
  }

  "div operator" should {
    "be accessible on ReqlNumber and" should {
      "allow to divide integer by integers" in {
        r.expr(123).div() shouldBe subtypeOf [ReqlFloat] and serializedTo("[27,[123]]")
        r.expr(123).div(234) shouldBe subtypeOf [ReqlFloat] and serializedTo("[27,[123,234]]")
        r.expr(123).div(234, 345) shouldBe subtypeOf [ReqlFloat] and serializedTo("[27,[123,234,345]]")
      }

      "allow to divide integer by floats" in {
        r.expr(123).div(BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123,234.234]]")

        r.expr(123).div(BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123,234.234,345.345]]")
      }

      "allow to divide integer by integers and floats" in {
        r.expr(123).div(BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123,234.234,345]]")

        r.expr(123).div(234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123,234,345.345]]")
      }

      "allow to divide float by floats" in {
        r.expr(BigDecimal(123.123)).div() shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123.123]]")

        r.expr(BigDecimal(123.123)).div(BigDecimal(234.234)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123.123,234.234]]")

        r.expr(BigDecimal(123.123)).div(BigDecimal(234.234), BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123.123,234.234,345.345]]")
      }

      "allow to divide float by integers" in {
        r.expr(BigDecimal(123.123)).div(234) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123.123,234]]")

        r.expr(BigDecimal(123.123)).div(234, 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123.123,234,345]]")
      }

      "allow to divide float by floats and integers" in {
        r.expr(BigDecimal(123.123)).div(234, BigDecimal(345.345)) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123.123,234,345.345]]")

        r.expr(BigDecimal(123.123)).div(BigDecimal(234.234), 345) shouldBe
          subtypeOf [ReqlFloat] and serializedTo("[27,[123.123,234.234,345]]")
      }
    }
  }

  "mod operator" should {
    "be accessible on ReqlInteger and" should {
      "not allow to call it without arguments" in {
        "r.expr(123).mod()" shouldNot compile
      }

      "allow to compute reminder after division of integer by integer" in {
        r.expr(123).mod(7) shouldBe subtypeOf [ReqlInteger] and serializedTo("[28,[123,7]]")
      }

      "not allow to call it with multiple arguments" in {
        "r.expr(123).mod(7, 4)" shouldNot compile
      }

      "not allow to divide integer by float" in {
        "r.expr(123).mod(4.3)" shouldNot compile
      }
    }

    "be not accessible on ReqlFloat" in {
      "r.expr(123.2).mod(4)" shouldNot compile
    }
  }
}
