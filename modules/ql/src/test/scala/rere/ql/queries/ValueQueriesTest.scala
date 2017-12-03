package rere.ql.queries

import java.util.UUID

import io.circe.{Json, JsonObject}
import org.scalatest.FlatSpec
import rere.ql.types._

class ValueQueriesTest extends FlatSpec with ReqlMatchers {

  import rere.ql.queries.all.r
  import rere.ql.queries.values._

  behavior of "ValueQueries"

  it should "convert basic types to ReQL types" in {
    r.expr(null) shouldBe subtypeOf [ReqlNull] and serializedTo("null")

    r.expr(true) shouldBe subtypeOf [ReqlBoolean] and serializedTo("true")
    r.expr(false) shouldBe subtypeOf [ReqlBoolean] and serializedTo("false")

    r.expr(123456) shouldBe subtypeOf [ReqlInteger] and serializedTo("123456")
    r.expr(123654L) shouldBe subtypeOf [ReqlInteger] and serializedTo("123654")
    r.expr(BigInt(345987L)) shouldBe subtypeOf [ReqlInteger] and serializedTo("345987")
    r.expr(123.456) shouldBe subtypeOf [ReqlFloat] and serializedTo("123.456")
    r.expr(Double.NaN) shouldBe subtypeOf [ReqlFloat] and serializedTo("null")
    r.expr(Double.PositiveInfinity) shouldBe subtypeOf [ReqlFloat] and serializedTo("null")
    r.expr(Double.NegativeInfinity) shouldBe subtypeOf [ReqlFloat] and serializedTo("null")
    r.expr(BigDecimal(123.456)) shouldBe subtypeOf [ReqlFloat] and serializedTo("123.456")

    r.expr("test \"string\"") shouldBe subtypeOf [ReqlString] and serializedTo(""""test \"string\""""")

    r.expr(UUID.fromString("e0c568f6-a901-4ebd-a373-53908b28c2f8")) shouldBe
      subtypeOf [ReqlUUID] and serializedTo(""""e0c568f6-a901-4ebd-a373-53908b28c2f8"""")

    r.expr(Seq(r.expr(null), r.expr(true), r.expr(4815162342L), r.expr("yep"))) shouldBe
      subtypeOf [ReqlArray[ReqlDatum]] and serializedTo("""[2,[null,true,4815162342,"yep"]]""")

    r.expr(List(Json.fromString("test"))) shouldBe
      subtypeOf [ReqlArray[ReqlJson]] and serializedTo("""[2,["test"]]""")

    r.expr(JsonObject.fromIterable(Seq(
      "code" -> Json.fromValues(Seq(
        Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)
      ))
    ))) shouldBe subtypeOf [ReqlJsonObject] and serializedTo("""{"code":[2,[1,2,3]]}""")

    r.expr(JsonObject.fromIterable(Seq(
      "code" -> Json.fromInt(123),
      "name" -> Json.fromValues(Seq(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
    ))) shouldBe subtypeOf [ReqlJsonObject] and serializedTo("""{"code":123,"name":[2,[1,2,3]]}""")

    r.expr(Map(
      "code" -> r.expr(123),
      "name" -> r.expr(Seq(r.expr(1), r.expr(2), r.expr(3)))
    )) shouldBe subtypeOf [ReqlObject] and serializedTo("""{"code":123,"name":[2,[1,2,3]]}""")

    r.expr(Map(
      "co\"de" -> r.expr(123),
      "na\\me" -> r.expr(Seq(r.expr(1), r.expr(2), r.expr(3)))
    )) shouldBe subtypeOf [ReqlObject] and serializedTo("""{"co\"de":123,"na\\me":[2,[1,2,3]]}""")

    r.expr(Map(
      "code" -> r.expr(123),
      "name" -> r.expr(Seq(r.expr(1), r.expr(JsonObject.fromMap(Map(
        "test" -> Json.fromValues(Seq(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
      ))), r.expr(3)))
    )) shouldBe
      subtypeOf [ReqlObject] and
      serializedTo("""{"code":123,"name":[2,[1,{"test":[2,[1,2,3]]},3]]}""")

    r.expr(Map(
      "test" -> r.expr(Seq(r.expr(1), r.expr(2))),
      "test2" -> r.expr(Seq(r.expr(3), r.expr(Map(
        "test3" -> r.expr(Seq(r.expr(4), r.expr(Seq(r.expr(5), r.expr(6)))))
      ))))
    )) shouldBe
      subtypeOf [ReqlObject] and
      serializedTo("""{"test":[2,[1,2]],"test2":[2,[3,{"test3":[2,[4,[2,[5,6]]]]}]]}""")

    r.expr(
      Json.fromValues(Seq(Json.fromInt(123), Json.fromString("te\"st"), Json.fromBoolean(false)))
    ) shouldBe subtypeOf [ReqlJson] and serializedTo("""[2,[123,"te\"st",false]]""")
  }

  it should "implicitly convert some types to ReQL types" in {
    import io.circe.literal._

    r.expr(Seq(1, 2, 3): Seq[ReqlInteger]) shouldBe
      subtypeOf [ReqlArray[ReqlInteger]] and serializedTo("[2,[1,2,3]]")

    r.expr(Map("te\"st" -> "test value"): Map[String, ReqlValue]) shouldBe
      subtypeOf [ReqlObject] and serializedTo("""{"te\"st":"test value"}""")

    forceConversion(json"""{"abc": [1, [2, {"bcd": null}]]}""") shouldBe
      subtypeOf[ReqlJson] and serializedTo("""{"abc":[2,[1,[2,[2,{"bcd":null}]]]]}""")

    (json"""{"abc": [1, [2, {"bcd": null}]]}""": ReqlJson) shouldBe
      subtypeOf[ReqlJson] and serializedTo("""{"abc":[2,[1,[2,[2,{"bcd":null}]]]]}""")
  }
}
