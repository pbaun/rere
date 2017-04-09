package rere.ql.queries

import org.scalatest.FlatSpec
import rere.ql.types.ReqlBoolean

class LogicQueriesTest extends FlatSpec with ReqlMatchers {

  import rere.ql.queries.all.r
  import rere.ql.queries.logic._
  import rere.ql.queries.values._

  behavior of "LogicQueries"

  it should "provide .or operator on r" in {
    r.or() shouldBe subtypeOf [ReqlBoolean] and serializedTo("""[66,[]]""")

    r.or(r.expr(true)) shouldBe subtypeOf [ReqlBoolean] and serializedTo("""[66,[true]]""")

    r.or(r.expr(true), r.expr(123).gt(5)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[66,[true,[21,[123,5]]]]""")

    r.or(r.expr(true), r.expr(123).gt(5), r.expr(false)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[66,[true,[21,[123,5]],false]]""")
  }

  it should "provide .or operator on ReqlBoolean" in {
    r.expr(true).or() shouldBe subtypeOf [ReqlBoolean] and serializedTo("""[66,[true]]""")

    r.expr(true).or(r.expr(123).gt(5)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[66,[true,[21,[123,5]]]]""")

    r.expr(true).or(r.expr(123).gt(5), r.expr(false)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[66,[true,[21,[123,5]],false]]""")
  }

  it should "provide .and operator on r" in {
    r.and() shouldBe subtypeOf [ReqlBoolean] and serializedTo("""[67,[]]""")

    r.and(r.expr(true)) shouldBe subtypeOf [ReqlBoolean] and serializedTo("""[67,[true]]""")

    r.and(r.expr(true), r.expr(123).gt(5)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[67,[true,[21,[123,5]]]]""")

    r.and(r.expr(true), r.expr(123).gt(5), r.expr(false)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[67,[true,[21,[123,5]],false]]""")
  }

  it should "provide .and operator on ReqlBoolean" in {
    r.expr(true).and() shouldBe subtypeOf [ReqlBoolean] and serializedTo("""[67,[true]]""")

    r.expr(true).and(r.expr(123).gt(5)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[67,[true,[21,[123,5]]]]""")

    r.expr(true).and(r.expr(123).gt(5), r.expr(false)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[67,[true,[21,[123,5]],false]]""")
  }

  it should "provide .eq operator on r" in {
    r.eq_(r.expr(123), r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[17,[123,234,345]]")
  }

  it should "provide .eq operator on ReqlDatum" in {
    r.expr(123).eq_(r.expr(234)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[17,[123,234]]")

    r.expr(123).eq_(r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[17,[123,234,345]]")

    r.expr("a").eq_(r.expr("b"), r.expr("c")) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[17,["a","b","c"]]""")
  }

  it should "provide .ne operator on r" in {
    r.ne_(r.expr(123), r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[18,[123,234,345]]")
  }

  it should "provide .ne operator on ReqlDatum" in {
    r.expr(123).ne_(r.expr(234)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[18,[123,234]]")

    r.expr(123).ne_(r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[18,[123,234,345]]")

    r.expr("a").ne_(r.expr("b"), r.expr("c")) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[18,["a","b","c"]]""")
  }

  it should "provide .lt operator on r" in {
    r.lt(r.expr(123), r.expr(234), r.expr(345)) shouldBe
      subtypeOf[ReqlBoolean] and serializedTo("[19,[123,234,345]]")
  }

  it should "provide .lt operator on ReqlDatum" in {
    r.expr(123).lt(r.expr(234)) shouldBe
      subtypeOf[ReqlBoolean] and serializedTo("[19,[123,234]]")

    r.expr(123).lt(r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[19,[123,234,345]]")

    r.expr("a").lt(r.expr("b"), r.expr("c")) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[19,["a","b","c"]]""")
  }

  it should "provide .le operator on r" in {
    r.le(r.expr(123), r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[20,[123,234,345]]")
  }

  it should "provide .le operator on ReqlDatum" in {
    r.expr(123).le(r.expr(234)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[20,[123,234]]")

    r.expr(123).le(r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[20,[123,234,345]]")

    r.expr("a").le(r.expr("b"), r.expr("c")) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[20,["a","b","c"]]""")
  }


  it should "provide .gt operator on r" in {
    r.gt(r.expr(123), r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[21,[123,234,345]]")
  }

  it should "provide .gt operator on ReqlDatum" in {
    r.expr(123).gt(r.expr(234)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[21,[123,234]]")

    r.expr(123).gt(r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[21,[123,234,345]]")

    r.expr("a").gt(r.expr("b"), r.expr("c")) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[21,["a","b","c"]]""")
  }

  it should "provide .ge operator on r" in {
    r.ge(r.expr(123), r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[22,[123,234,345]]")
  }

  it should "provide .ge operator on ReqlDatum" in {
    r.expr(123).ge(r.expr(234)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[22,[123,234]]")

    r.expr(123).ge(r.expr(234), r.expr(345)) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[22,[123,234,345]]")

    r.expr("a").ge(r.expr("b"), r.expr("c")) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("""[22,["a","b","c"]]""")
  }

  it should "provide .not operator on ReqlBoolean" in {
    r.not(false) shouldBe subtypeOf [ReqlBoolean] and serializedTo("[23,[false]]")
    r.not(true) shouldBe subtypeOf [ReqlBoolean] and serializedTo("[23,[true]]")

    r.expr(false).not() shouldBe subtypeOf [ReqlBoolean] and serializedTo("[23,[false]]")
    r.expr(true).not() shouldBe subtypeOf [ReqlBoolean] and serializedTo("[23,[true]]")

    r.not(false).not() shouldBe subtypeOf [ReqlBoolean] and serializedTo("[23,[[23,[false]]]]")
    r.expr(false).not().not() shouldBe subtypeOf [ReqlBoolean] and serializedTo("[23,[[23,[false]]]]")

    r.expr(123).gt(234).not() shouldBe subtypeOf [ReqlBoolean] and serializedTo("[23,[[21,[123,234]]]]")

    r.eq_(
      r.expr(123).le(234),
      r.ge(r.expr(123), r.expr(234)).not()
    ) shouldBe subtypeOf [ReqlBoolean] and serializedTo("[17,[[20,[123,234]],[23,[[22,[123,234]]]]]]")

    r.not(r.gt(r.expr(123), r.expr(234))) shouldBe
      subtypeOf [ReqlBoolean] and serializedTo("[23,[[21,[123,234]]]]")
  }

}
