package rere.ql.queries.dsl

import akka.util.ByteString
import io.circe.{Json, JsonObject}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import rere.ql.queries.{DSLKeyValuePair, DSLKeyValuePairList}
import rere.ql.rasterization
import rere.ql.types._

class StructuresDSL extends FlatSpec {

  import rere.ql.queries.all._

  behavior of "dsl"

  it should "produce pair with basic types" in {
    ("key" := "value") shouldBe an[DSLKeyValuePair]
    ("key" := 123) shouldBe an[DSLKeyValuePair]
    ("key" := false) shouldBe an[DSLKeyValuePair]
    ("key" := 777L) shouldBe an[DSLKeyValuePair]
  }

  it should "produce pair with pseude types" in {
    (r.expr("key") := r.now()) shouldBe an[DSLKeyValuePair]
    (r.expr("key") := r.binary(ByteString(1, 2, 3, 4))) shouldBe an[DSLKeyValuePair]
    (r.uuid() := r.point(BigDecimal(1.2), BigDecimal(2.5))) shouldBe an[DSLKeyValuePair]
  }

  it should "compose pairs" in {
    ("key" := "value") ~ ("key2" := 123) shouldBe an[DSLKeyValuePairList]
    ("key" := "value") ~ ("key2" := 123) ~ ("key3" := true) shouldBe an[DSLKeyValuePairList]
  }

  it should "make reql object from pair of pair composition" in {
    def reqlObjectSerializer(obj: ReqlObject): String = {
      obj shouldBe an[ReqlObject]

      import rasterization.building._
      obj.build().utf8String
    }

    reqlObjectSerializer("key" := "value") shouldBe """{"key":"value"}"""

    reqlObjectSerializer("""k"ey""" := """va"lue""") shouldBe """{"k\"ey":"va\"lue"}"""

    reqlObjectSerializer(("key" := "value") ~ ("key2" := 123)) shouldBe """{"key":"value","key2":123}"""

    reqlObjectSerializer(
      ("key" := "value") ~
      ("key2" := 123) ~
      ("key3" := true)
    ) shouldBe """{"key":"value","key2":123,"key3":true}"""

    reqlObjectSerializer(
      ("key" := "value") ~
      ("obj" :=
        ("key2" := 123L) ~
        ("key3" := false)
      ) ~
      ("key4" := true)
    ) shouldBe """{"key":"value","obj":{"key2":123,"key3":false},"key4":true}"""
  }

  trait Types {
    //https://github.com/scalatest/scalatest/issues/973
    import scala.reflect.runtime.universe.TypeTag
    import org.scalatest.matchers._

    def getType[T](x: T)(implicit tag: TypeTag[T]): String = tag.tpe.toString

    class TypeMatcher[S, D](implicit source: TypeTag[S], dest: TypeTag[D]) extends Matcher[S] {
      def apply(left: S): MatchResult = MatchResult(
        source.tpe <:< dest.tpe,
        s"value $left has expected type ${dest.tpe}",
        s"value $left has type ${source.tpe}, but ${dest.tpe} expected"
      )
    }

    class TypeMatcherProvider[D](implicit dest: TypeTag[D]) {
      def get[S](implicit source: TypeTag[S]): TypeMatcher[S, D] = {
        new TypeMatcher()
      }
    }

    def hasType[D](implicit dest: TypeTag[D]): TypeMatcherProvider[D] = {
      new TypeMatcherProvider()
    }

    implicit def useMatcherProvider[S,D](from: TypeMatcherProvider[D])(implicit source: TypeTag[S]): Matcher[S] = {
      from.get[S](source)
    }
  }
  object Types extends Types

  it should "allow construct typed list with right type" in {
    import Types._

    val a0: ReqlArray[ReqlNull] = null :: ReNil
    val a1: ReqlArray[ReqlBoolean] = true :: ReNil
    val a2: ReqlArray[ReqlInteger] = 1 :: ReNil
    val a3: ReqlArray[ReqlInteger] = 123L :: ReNil
    val a4: ReqlArray[ReqlInteger] = BigInt(234) :: ReNil
    val a5: ReqlArray[ReqlFloat] = BigDecimal(345.678) :: ReNil
    val a6: ReqlArray[ReqlString] = "abc" :: ReNil
    val a7: ReqlArray[ReqlArray[ReqlString]] = ("abc" :: ReNil) :: ReNil
    val a8: ReqlArray[ReqlArray[ReqlJson]] = (Json.fromString("abc") :: Nil) :: ReNil
    val a9: ReqlArray[ReqlObject] = ("a" := 123) :: ReNil
    val a10: ReqlArray[ReqlObject] = JsonObject.empty :: ReNil
    val a11: ReqlArray[ReqlJson] = Json.fromString("abc") :: ReNil
    val a12: ReqlArray[ReqlDatum] = (123: ReqlValue) :: ReNil

    val b0: ReqlArray[ReqlNull] = null :: null :: ReNil
    val b1: ReqlArray[ReqlNull] = null :: null :: null :: ReNil
    val b2: ReqlArray[ReqlDatum] = true :: null :: ReNil
    val b3: ReqlArray[ReqlDatum] = 1 :: null :: ReNil
    val b4: ReqlArray[ReqlDatum] = 1L :: null :: ReNil
    val b5: ReqlArray[ReqlDatum] = BigInt(1) :: null :: ReNil
    val b6: ReqlArray[ReqlDatum] = BigDecimal(1) :: null :: ReNil
    val b7: ReqlArray[ReqlDatum] = "abc" :: null :: ReNil
    val b8: ReqlArray[ReqlDatum] = (1 :: ReNil) :: null :: ReNil
    val b9: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: null :: ReNil
    val b10: ReqlArray[ReqlDatum] = ("a" := 123) :: null :: ReNil
    val b11: ReqlArray[ReqlDatum] = JsonObject.empty :: null :: ReNil
    val b12: ReqlArray[ReqlDatum] = Json.fromString("abc") :: null :: ReNil
    val b13: ReqlArray[ReqlDatum] = (123: ReqlValue) :: null :: ReNil

    val c0: ReqlArray[ReqlDatum] = null :: false :: ReNil
    val c1: ReqlArray[ReqlBoolean] = true :: false :: ReNil
    val c2: ReqlArray[ReqlBoolean] = false :: true :: false :: ReNil
    val c3: ReqlArray[ReqlDatum] = 1 :: false :: ReNil
    val c4: ReqlArray[ReqlDatum] = 1L :: false :: ReNil
    val c5: ReqlArray[ReqlDatum] = BigInt(1) :: false :: ReNil
    val c6: ReqlArray[ReqlDatum] = BigDecimal(1) :: false :: ReNil
    val c7: ReqlArray[ReqlDatum] = "abc" :: false :: ReNil
    val c8: ReqlArray[ReqlDatum] = (1 :: ReNil) :: false :: ReNil
    val c9: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: false :: ReNil
    val c10: ReqlArray[ReqlDatum] = ("a" := 123) :: false :: ReNil
    val c11: ReqlArray[ReqlDatum] = JsonObject.empty :: false :: ReNil
    val c12: ReqlArray[ReqlDatum] = Json.fromString("abc") :: false :: ReNil
    val c13: ReqlArray[ReqlDatum] = (123: ReqlValue) :: false :: ReNil

    val d0: ReqlArray[ReqlDatum] = null :: 1 :: ReNil
    val d1: ReqlArray[ReqlDatum] = false :: 1 :: ReNil
    val d2: ReqlArray[ReqlInteger] = 1 :: 2 :: ReNil
    val d3: ReqlArray[ReqlInteger] = 0 :: 1 :: 2 :: ReNil
    val d4: ReqlArray[ReqlInteger] = 1L :: 2 :: ReNil
    val d5: ReqlArray[ReqlInteger] = BigInt(1) :: 2 :: ReNil
    val d6: ReqlArray[ReqlNumber] = BigDecimal(1) :: 2 :: ReNil
    val d7: ReqlArray[ReqlDatum] = "abc" :: 2 :: ReNil
    val d8: ReqlArray[ReqlDatum] = (1 :: ReNil) :: 2 :: ReNil
    val d9: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: 2 :: ReNil
    val d10: ReqlArray[ReqlDatum] = ("a" := 123) :: 2 :: ReNil
    val d11: ReqlArray[ReqlDatum] = JsonObject.empty :: 2 :: ReNil
    val d12: ReqlArray[ReqlDatum] = Json.fromString("abc") :: 2 :: ReNil
    val d13: ReqlArray[ReqlDatum] = (123: ReqlValue) :: 2 :: ReNil

    val e0: ReqlArray[ReqlDatum] = null :: 1L :: ReNil
    val e1: ReqlArray[ReqlDatum] = false :: 1L :: ReNil
    val e2: ReqlArray[ReqlInteger] = 1 :: 2L :: ReNil
    val e3: ReqlArray[ReqlInteger] = 1L :: 2L :: ReNil
    val e4: ReqlArray[ReqlInteger] = 0L :: 1L :: 2L :: ReNil
    val e5: ReqlArray[ReqlInteger] = BigInt(1) :: 2L :: ReNil
    val e6: ReqlArray[ReqlNumber] = BigDecimal(1) :: 2L :: ReNil
    val e7: ReqlArray[ReqlDatum] = "abc" :: 2L :: ReNil
    val e8: ReqlArray[ReqlDatum] = (1 :: ReNil) :: 2L :: ReNil
    val e9: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: 2L :: ReNil
    val e10: ReqlArray[ReqlDatum] = ("a" := 123) :: 2L :: ReNil
    val e11: ReqlArray[ReqlDatum] = JsonObject.empty :: 2L :: ReNil
    val e12: ReqlArray[ReqlDatum] = Json.fromString("abc") :: 2L :: ReNil
    val e13: ReqlArray[ReqlDatum] = (123: ReqlValue) :: 2L :: ReNil

    val f0: ReqlArray[ReqlDatum] = null :: BigInt(1) :: ReNil
    val f1: ReqlArray[ReqlDatum] = false :: BigInt(1) :: ReNil
    val f2: ReqlArray[ReqlInteger] = 1 :: BigInt(2) :: ReNil
    val f3: ReqlArray[ReqlInteger] = 1L :: BigInt(2) :: ReNil
    val f4: ReqlArray[ReqlInteger] = BigInt(1) :: BigInt(2) :: ReNil
    val f5: ReqlArray[ReqlInteger] = BigInt(0) :: BigInt(1) :: BigInt(2):: ReNil
    val f6: ReqlArray[ReqlNumber] = BigDecimal(1) :: BigInt(2) :: ReNil
    val f7: ReqlArray[ReqlDatum] = "abc" :: BigInt(2) :: ReNil
    val f8: ReqlArray[ReqlDatum] = (1 :: ReNil) :: BigInt(2) :: ReNil
    val f9: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: BigInt(2) :: ReNil
    val f10: ReqlArray[ReqlDatum] = ("a" := 123) :: BigInt(2) :: ReNil
    val f11: ReqlArray[ReqlDatum] = JsonObject.empty :: BigInt(2) :: ReNil
    val f12: ReqlArray[ReqlDatum] = Json.fromString("abc") :: BigInt(2) :: ReNil
    val f13: ReqlArray[ReqlDatum] = (123: ReqlValue) :: BigInt(2) :: ReNil

    val g0: ReqlArray[ReqlDatum] = null :: BigDecimal(1) :: ReNil
    val g1: ReqlArray[ReqlDatum] = false :: BigDecimal(1) :: ReNil
    val g2: ReqlArray[ReqlNumber] = 1 :: BigDecimal(2) :: ReNil
    val g3: ReqlArray[ReqlNumber] = 1L :: BigDecimal(2) :: ReNil
    val g4: ReqlArray[ReqlNumber] = BigInt(1) :: BigDecimal(2) :: ReNil
    val g5: ReqlArray[ReqlFloat] = BigDecimal(1) :: BigDecimal(2) :: ReNil
    val g6: ReqlArray[ReqlFloat] = BigDecimal(0) :: BigDecimal(1) :: BigDecimal(2) :: ReNil
    val g7: ReqlArray[ReqlDatum] = "abc" :: BigDecimal(2) :: ReNil
    val g8: ReqlArray[ReqlDatum] = (1 :: ReNil) :: BigDecimal(2) :: ReNil
    val g9: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: BigDecimal(2) :: ReNil
    val g10: ReqlArray[ReqlDatum] = ("a" := 123) :: BigDecimal(2) :: ReNil
    val g11: ReqlArray[ReqlDatum] = JsonObject.empty :: BigDecimal(2) :: ReNil
    val g12: ReqlArray[ReqlDatum] = Json.fromString("abc") :: BigDecimal(2) :: ReNil
    val g13: ReqlArray[ReqlDatum] = (123: ReqlValue):: BigDecimal(2) :: ReNil

    val h: ReConsNumber = 2 :: BigDecimal(3) :: ReNil
    val h0: ReqlArray[ReqlDatum] = null :: h
    val h1: ReqlArray[ReqlDatum] = false :: h
    val h2: ReqlArray[ReqlNumber] = 1 :: h
    val h3: ReqlArray[ReqlNumber] = 1L :: h
    val h4: ReqlArray[ReqlNumber] = BigInt(1) :: h
    val h5: ReqlArray[ReqlNumber] = BigDecimal(1) :: h
    val h6: ReqlArray[ReqlDatum] = "abc" :: h
    val h7: ReqlArray[ReqlDatum] = (1 :: ReNil) :: h
    val h8: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: h
    val h9: ReqlArray[ReqlDatum] = ("a" := 123) :: h
    val h10: ReqlArray[ReqlDatum] = JsonObject.empty :: h
    val h11: ReqlArray[ReqlDatum] = Json.fromString("abc") :: h
    val h12: ReqlArray[ReqlDatum] = (123: ReqlValue) :: h

    val i0: ReqlArray[ReqlDatum] = null :: "abc" :: ReNil
    val i1: ReqlArray[ReqlDatum] = false :: "abc" :: ReNil
    val i2: ReqlArray[ReqlDatum] = 1 :: "abc" :: ReNil
    val i3: ReqlArray[ReqlDatum] = 1L :: "abc" :: ReNil
    val i4: ReqlArray[ReqlDatum] = BigInt(1) :: "abc" :: ReNil
    val i5: ReqlArray[ReqlDatum] = BigDecimal(1) :: "abc" :: ReNil
    val i6: ReqlArray[ReqlString] = "abc" :: "bcd" :: ReNil
    val i7: ReqlArray[ReqlString] = "abc" :: "bcd" :: "cde" :: ReNil
    val i8: ReqlArray[ReqlDatum] = (1 :: ReNil) :: "cde" :: ReNil
    val i9: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: "cde" :: ReNil
    val i10: ReqlArray[ReqlDatum] = ("a" := 123) :: "cde" :: ReNil
    val i11: ReqlArray[ReqlDatum] = JsonObject.empty :: "cde" :: ReNil
    val i12: ReqlArray[ReqlDatum] = Json.fromString("abc") :: "cde" :: ReNil
    val i13: ReqlArray[ReqlDatum] = (123: ReqlValue) :: "cde" :: ReNil

    val j: ReConsArray[ReqlInteger] = (1 :: 2 :: ReNil) :: ReNil
    val j0: ReqlArray[ReqlDatum] = null :: j
    val j1: ReqlArray[ReqlDatum] = false :: j
    val j2: ReqlArray[ReqlDatum] = 1 :: j
    val j3: ReqlArray[ReqlDatum] = 1L :: j
    val j4: ReqlArray[ReqlDatum] = BigInt(1) :: j
    val j5: ReqlArray[ReqlDatum] = BigDecimal(1) :: j
    val j6: ReqlArray[ReqlDatum] = "abc" :: j
    val j7: ReqlArray[ReqlArray[ReqlInteger]] = (1 :: ReNil) :: j
    val j8: ReqlArray[ReqlArray[ReqlInteger]] = (1 :: ReNil) :: (2 :: ReNil) :: j
    val j9: ReqlArray[ReqlArray[ReqlNumber]] = (BigDecimal(1) :: ReNil) :: j
    val j10: ReqlArray[ReqlArray[ReqlDatum]] = ("abc" :: ReNil) :: j
    val j11: ReqlArray[ReqlArray[ReqlDatum]] = (Json.fromString("abc") :: Nil) :: j
    val j12: ReqlArray[ReqlArray[ReqlJson]] = (Json.fromString("abc") :: Nil) :: (Json.fromString("bcd") :: Nil) :: ReNil
    val j13: ReqlArray[ReqlDatum] = ("a" := 123) :: (Json.fromString("bcd") :: Nil) :: ReNil
    val j14: ReqlArray[ReqlDatum] = JsonObject.empty :: (Json.fromString("bcd") :: Nil) :: ReNil
    val j15: ReqlArray[ReqlDatum] = Json.fromString("abc") :: j
    val j16: ReqlArray[ReqlDatum] = (123: ReqlValue) :: j

    (("abc" :: ReNil) :: j) should hasType[ReqlArray[ReqlArray[ReqlDatum]]]
    ((Json.fromString("abc") :: Nil) :: j) should hasType[ReqlArray[ReqlArray[ReqlDatum]]]
    ((Json.fromString("abc") :: Nil) :: j) shouldNot hasType[ReqlArray[ReqlArray[ReqlInteger]]]
    ((Json.fromString("abc") :: Nil) :: (Json.fromString("bcd") :: Nil) :: ReNil) should hasType[ReqlArray[ReqlArray[ReqlJson]]]
    getType((Json.fromString("abc") :: Nil) :: (Json.fromString("bcd") :: Nil) :: ReNil) should include("Json")

    val k: ReConsObject = ("a" := 123) :: ReNil
    val kk: ReConsObject = JsonObject.empty :: ReNil
    val k0: ReqlArray[ReqlDatum] = null :: k
    val k1: ReqlArray[ReqlDatum] = false :: k
    val k2: ReqlArray[ReqlDatum] = 1 :: k
    val k3: ReqlArray[ReqlDatum] = 1L :: k
    val k4: ReqlArray[ReqlDatum] = BigInt(1) :: k
    val k5: ReqlArray[ReqlDatum] = BigDecimal(1) :: k
    val k6: ReqlArray[ReqlDatum] = "abc" :: k
    val k7: ReqlArray[ReqlDatum] = (1 :: ReNil) :: k
    val k8: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: k
    val k9: ReqlArray[ReqlObject] = ("a" := 123) :: k
    val k10: ReqlArray[ReqlObject] = ("a" := 123) :: ("b" := 234) :: k
    val k11: ReqlArray[ReqlObject] = JsonObject.empty :: k
    val k12: ReqlArray[ReqlObject] = JsonObject.empty :: JsonObject.empty :: k
    val k13: ReqlArray[ReqlObject] = ("a" := 123) :: JsonObject.empty :: k
    val k14: ReqlArray[ReqlDatum] = Json.fromString("abc") :: k
    val k15: ReqlArray[ReqlDatum] = (123: ReqlValue) :: k

    val l: ReConsJson = Json.fromString("abc") :: ReNil
    val l0: ReqlArray[ReqlDatum] = null :: l
    val l1: ReqlArray[ReqlDatum] = false :: l
    val l2: ReqlArray[ReqlDatum] = 1 :: l
    val l3: ReqlArray[ReqlDatum] = 1L :: l
    val l4: ReqlArray[ReqlDatum] = BigInt(1) :: l
    val l5: ReqlArray[ReqlDatum] = BigDecimal(1) :: l
    val l6: ReqlArray[ReqlDatum] = "abc" :: l
    val l7: ReqlArray[ReqlDatum] = (1 :: ReNil) :: l
    val l8: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: l
    val l9: ReqlArray[ReqlDatum] = ("a" := 123) :: l
    val l10: ReqlArray[ReqlDatum] = JsonObject.empty :: l
    val l11: ReqlArray[ReqlJson] = Json.fromString("abc") :: l
    val l12: ReqlArray[ReqlJson] = Json.fromString("abc") :: Json.fromString("bcd") :: l
    val l13: ReqlArray[ReqlDatum] = (123: ReqlValue) :: l

    val z: ReConsDatum = null :: 1 :: ReNil
    val z0: ReqlArray[ReqlDatum] = null :: z
    val z1: ReqlArray[ReqlDatum] = false :: z
    val z2: ReqlArray[ReqlDatum] = 1 :: z
    val z3: ReqlArray[ReqlDatum] = 1L :: z
    val z4: ReqlArray[ReqlDatum] = BigInt(1) :: z
    val z5: ReqlArray[ReqlDatum] = BigDecimal(1) :: z
    val z6: ReqlArray[ReqlDatum] = "abc" :: z
    val z7: ReqlArray[ReqlDatum] = (1 :: ReNil) :: z
    val z8: ReqlArray[ReqlDatum] = (Json.fromString("abc") :: Nil) :: z
    val z9: ReqlArray[ReqlDatum] = ("a" := 123) :: z
    val z10: ReqlArray[ReqlDatum] = JsonObject.empty :: z
    val z11: ReqlArray[ReqlDatum] = Json.fromString("abc") :: z
    val z12: ReqlArray[ReqlDatum] = (123: ReqlValue) :: z
    val z13: ReqlArray[ReqlDatum] = (123: ReqlValue) :: (234: ReqlValue) :: z
  }
}
