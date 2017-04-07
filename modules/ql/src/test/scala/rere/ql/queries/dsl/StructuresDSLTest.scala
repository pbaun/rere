package rere.ql.queries.dsl

import akka.util.ByteString
import io.circe.{Json, JsonObject}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import rere.ql.queries.{DSLKeyValuePair, DSLKeyValuePairList, ReqlMatchers}
import rere.ql.types._

class StructuresDSLTest extends FlatSpec with ReqlMatchers {

  import rere.ql.queries.all._

  behavior of "object dsl"

  it should "produce pair with basic types" in {
    ("key" := "value") shouldBe subtypeOf [DSLKeyValuePair] and serializedTo("""{"key":"value"}""")
    ("key" := 123) shouldBe subtypeOf [DSLKeyValuePair] and serializedTo("""{"key":123}""")
    ("key" := false) shouldBe subtypeOf [DSLKeyValuePair] and serializedTo("""{"key":false}""")
    ("key" := 777L) shouldBe subtypeOf [DSLKeyValuePair] and serializedTo("""{"key":777}""")
  }

  it should "produce pair with pseudo types" in {
    (r.expr("key") := r.now()) shouldBe subtypeOf [DSLKeyValuePair] and serializedTo("""{"key":[103,[]]}""")

    (r.expr("key") := r.binary(ByteString(1, 2, 3, 4))) shouldBe
      subtypeOf [DSLKeyValuePair] and serializedTo("""{"key":{"$reql_type$":"BINARY","data":"AQIDBA=="}}""")

    (r.uuid() := r.point(BigDecimal(1.2), BigDecimal(2.5))) shouldBe
      subtypeOf [DSLKeyValuePair] and serializedTo("""{[169,[]]:[159,[1.2,2.5]]}""")
  }

  it should "compose pairs" in {
    ("key" := "value") ~ ("key2" := 123) shouldBe subtypeOf [DSLKeyValuePairList]
    ("key" := "value") ~ ("key2" := 123) ~ ("key3" := true) shouldBe subtypeOf [DSLKeyValuePairList]
  }

  it should "make reql object from pair of pair composition" in {
    ("key" := "value") shouldBe serializedTo("""{"key":"value"}""")

    ("""k"ey""" := """va"lue""") shouldBe serializedTo("""{"k\"ey":"va\"lue"}""")

    (("key" := "value") ~ ("key2" := 123)) shouldBe serializedTo("""{"key":"value","key2":123}""")

    (
      ("key" := "value") ~
      ("key2" := 123) ~
      ("key3" := true)
    ) shouldBe serializedTo("""{"key":"value","key2":123,"key3":true}""")

    (
      ("key" := "value") ~
      ("obj" :=
        ("key2" := 123L) ~
        ("key3" := false)
      ) ~
      ("key4" := true)
    ) shouldBe serializedTo("""{"key":"value","obj":{"key2":123,"key3":false},"key4":true}""")
  }


  behavior of "list dsl"

  it should "allow to construct single element list with right type" in {
    (null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlNull]]

    (true :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlBoolean]]

    (1 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (123L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (BigInt(234) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (BigDecimal(345.678) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlFloat]]

    ("abc" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlString]]

    (("abc" :: ReNil) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlString]]]

    ((Json.fromString("abc") :: Nil) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlJson]]]

    (("a" := 123) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlObject]]

    (JsonObject.empty :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlObject]]

    (Json.fromString("abc") :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlJson]]

    ((123: ReqlValue) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing 'Null' element and infer right LUB for result" in {
    (null :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlNull]]

    (null :: null :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlNull]]

    (true :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1L :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigInt(1) :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigDecimal(1) :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ("abc" :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: null :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing 'Boolean' element and infer right LUB for result" in {
    (null :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (true :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlBoolean]]

    (false :: true :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlBoolean]]

    (1 :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1L :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigInt(1) :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigDecimal(1) :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ("abc" :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: false :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing 'Integer' element and infer right LUB for result" in {
    (null :: 1 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: 1 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (0 :: 1 :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (1L :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (BigInt(1) :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (BigDecimal(1) :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    ("abc" :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: 2 :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing 'Long' element and infer right LUB for result" in {
    (null :: 1L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: 1L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (1L :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (0L :: 1L :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (BigInt(1) :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (BigDecimal(1) :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    ("abc" :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: 2L :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing 'BigInt' element and infer right LUB for result" in {
    (null :: BigInt(1) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: BigInt(1) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (1L :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (BigInt(1) :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (BigInt(0) :: BigInt(1) :: BigInt(2):: ReNil) shouldBe subtypeOf [ReqlArray[ReqlInteger]]

    (BigDecimal(1) :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    ("abc" :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: BigInt(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing 'BigDecimal' element and infer right LUB for result" in {
    (null :: BigDecimal(1) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: BigDecimal(1) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    (1L :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    (BigInt(1) :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    (BigDecimal(1) :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlFloat]]

    (BigDecimal(0) :: BigDecimal(1) :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlFloat]]

    ("abc" :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue):: BigDecimal(2) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing number elements and infer right LUB for result" in {
    val numbers: ReConsNumber = 2 :: BigDecimal(3) :: ReNil

    (null :: numbers) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: numbers) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: numbers) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    (1L :: numbers) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    (BigInt(1) :: numbers) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    (BigDecimal(1) :: numbers) shouldBe subtypeOf [ReqlArray[ReqlNumber]]

    ("abc" :: numbers) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: numbers) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: numbers) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: numbers) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: numbers) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: numbers) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: numbers) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing 'String' element and infer right LUB for result" in {
    (null :: "abc" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: "abc" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: "abc" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1L :: "abc" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigInt(1) :: "abc" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigDecimal(1) :: "abc" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ("abc" :: "bcd" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlString]]

    ("abc" :: "bcd" :: "cde" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlString]]

    ((1 :: ReNil) :: "cde" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: "cde" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: "cde" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: "cde" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: "cde" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: "cde" :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing another list and infer right LUB for result" in {
    val listOfLists: ReConsArray[ReqlInteger] = (1 :: 2 :: ReNil) :: ReNil

    (null :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1L :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigInt(1) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigDecimal(1) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ("abc" :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlInteger]]]

    ((1 :: ReNil) :: (2 :: ReNil) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlInteger]]]

    ((BigDecimal(1) :: ReNil) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlNumber]]]

    (("abc" :: ReNil) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlDatum]]]

    ((Json.fromString("abc") :: Nil) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlDatum]]]

    ((Json.fromString("abc") :: Nil) :: (Json.fromString("bcd") :: Nil) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlJson]]]

    (("a" := 123) :: (Json.fromString("bcd") :: Nil) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: (Json.fromString("bcd") :: Nil) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlDatum]]


    (("abc" :: ReNil) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlDatum]]]

    ((Json.fromString("abc") :: Nil) :: listOfLists) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlDatum]]]

    """((Json.fromString("abc") :: Nil) :: j) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlInteger]]]""" shouldNot compile

    ((Json.fromString("abc") :: Nil) :: (Json.fromString("bcd") :: Nil) :: ReNil) shouldBe subtypeOf [ReqlArray[ReqlArray[ReqlJson]]]
  }

  it should "allow to prepend list containing 'ReqlObject' element and infer right LUB for result" in {
    val listOfObjects: ReConsObject = ("a" := 123) :: ReNil
    val anotherListOfObjects: ReConsObject = JsonObject.empty :: ReNil

    (null :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1L :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigInt(1) :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigDecimal(1) :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ("abc" :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlObject]]

    (("a" := 123) :: ("b" := 234) :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlObject]]

    (JsonObject.empty :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlObject]]

    (JsonObject.empty :: JsonObject.empty :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlObject]]

    (("a" := 123) :: JsonObject.empty :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlObject]]

    (Json.fromString("abc") :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: listOfObjects) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing 'ReqlJson' element and infer right LUB for result" in {
    val listOfJson: ReConsJson = Json.fromString("abc") :: ReNil

    (null :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1L :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigInt(1) :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigDecimal(1) :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ("abc" :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlJson]]

    (Json.fromString("abc") :: Json.fromString("bcd") :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlJson]]

    ((123: ReqlValue) :: listOfJson) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }

  it should "allow to prepend list containing elements of different types and infer right LUB for result" in {
    val mixedList: ReConsDatum = null :: 1 :: ReNil

    (null :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (false :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1 :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (1L :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigInt(1) :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (BigDecimal(1) :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ("abc" :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((1 :: ReNil) :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((Json.fromString("abc") :: Nil) :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (("a" := 123) :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (JsonObject.empty :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    (Json.fromString("abc") :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]

    ((123: ReqlValue) :: (234: ReqlValue) :: mixedList) shouldBe subtypeOf [ReqlArray[ReqlDatum]]
  }
}
