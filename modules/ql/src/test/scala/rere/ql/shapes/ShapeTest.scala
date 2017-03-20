package rere.ql.shapes

import java.nio.charset.StandardCharsets
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.util.UUID

import akka.util.ByteString
import io.circe.{Json, JsonObject}
import org.scalatest.{FlatSpec, Inside, Matchers}
import rere.ql.rasterization.ByteStringRenderer
import rere.ql.types.{ReqlObject, ReqlTable}

class ShapeTest extends FlatSpec with Matchers with Inside {

  case class Sample(
    flag: Boolean,
    i: Int,
    l: Long,
    bi: BigInt,
    d: Double,
    bd: BigDecimal,
    name: String,
    zonedDateTime: ZonedDateTime,
    uuid: UUID,
    json: Json,
    binary: ByteString,
    tag: Option[String],
    maybeDate: Option[ZonedDateTime],
    tags: Seq[String]
  )

  object SampleShape extends Shape(Sample.apply _) with IdeaTypeHint[Sample] {
    implicit val flag = field("flag", _.flag)
    implicit val i = field("i", _.i)
    implicit val l = field("l", _.l)
    implicit val bi = field("bi", _.bi)
    implicit val d = field("d", _.d)
    implicit val bd = field("bd", _.bd)
    implicit val name = field("name", _.name)
    implicit val zonedDateTime = field("zonedDateTime", _.zonedDateTime)
    implicit val uuid = field("uuid", _.uuid)
    implicit val json = field("json", _.json)
    implicit val binary = field("binary", _.binary)
    implicit val tag = field("tag", _.tag)
    implicit val maybeDate = field("maybeDate", _.maybeDate)
    implicit val tags = field("tags", _.tags)

    def projection: Projection = flag :-: i :-: l :-: bi :-: d :-: bd :-: name :-:
      zonedDateTime :-: uuid :-: json :-: binary :-: tag :-: maybeDate :-: tags :-: SNil
  }

  behavior of "Shape"

  it should "just work" in {
    val shape = SampleShape
    val instant = Instant.ofEpochSecond(531360000L)
    val date = ZonedDateTime.ofInstant(instant, ZoneOffset.UTC)
    val uuidValue = UUID.fromString("6c9d95fa-c234-42de-8d63-e7ebb2afeacf")
    val jsonValue: Json = Json.obj("a" -> Json.fromInt(123), "b" -> Json.fromString("test"))
    val binaryValue = ByteString("unicode string π")
    val tagsValue = Seq("s", "m", "p", "l")
    val model = Sample(true, 2, 3L, 12345, 123.45, 12345.67, "abc",
      date, uuidValue, jsonValue, binaryValue, Some("smpl"), Some(date), tagsValue)

    // Boolean
    shape.flag shouldBe an[shape.Field[_]]
    val flag: Boolean = shape.getField(model, "flag")
    inside(shape.getField(model, "flag")) { case true => }

    // Int
    shape.i shouldBe an[shape.Field[_]]
    val i: Int = shape.getField(model, "i")
    inside(shape.getField(model, "i")) { case 2 => }

    // Long
    shape.l shouldBe an[shape.Field[_]]
    val l: Long = shape.getField(model, "l")
    inside(shape.getField(model, "l")) { case 3L => }

    // BigInt
    shape.bi shouldBe an[shape.Field[_]]
    val bi: BigInt = shape.getField(model, "bi")
    inside(shape.getField(model, "bi")) { case x if x == BigInt(12345) => }

    // Double
    shape.d shouldBe an[shape.Field[_]]
    val d: Double = shape.getField(model, "d")
    inside(shape.getField(model, "d")) { case 123.45 => }

    // BigDecimal
    shape.bd shouldBe an[shape.Field[_]]
    val bd: BigDecimal = shape.getField(model, "bd")
    inside(shape.getField(model, "bd")) { case x if x == BigDecimal(12345.67) => }

    // String
    shape.name shouldBe an[shape.Field[_]]
    val name: String = shape.getField(model, "name")
    inside(shape.getField(model, "name")) { case "abc" => }

    // ZonedDateTime
    shape.zonedDateTime shouldBe an[shape.Field[_]]
    val zonedDateTime: ZonedDateTime = shape.getField(model, "zonedDateTime")
    inside(shape.getField(model, "zonedDateTime")) {
      case x: ZonedDateTime if x.toInstant == instant =>
    }

    // UUID
    shape.uuid shouldBe an[shape.Field[_]]
    val uuid: UUID = shape.getField(model, "uuid")
    inside(shape.getField(model, "uuid")) {
      case x: UUID if x == uuidValue =>
    }

    // JSON
    shape.json shouldBe an[shape.Field[_]]
    val json: Json = shape.getField(model, "json")
    inside(shape.getField(model, "json")) {
      case x: Json if x == jsonValue =>
    }

    // Binary
    shape.binary shouldBe an[shape.Field[_]]
    val binary: ByteString = shape.getField(model, "binary")
    inside(shape.getField(model, "binary")) {
      case x: ByteString if x == binaryValue =>
    }

    // Option[T]
    shape.tag shouldBe an[shape.Field[_]]
    val tag: Option[String] = shape.getField(model, "tag")
    inside(shape.getField(model, "tag")) { case Some("smpl") => }

    // Option[T] with complex T
    shape.maybeDate shouldBe an[shape.Field[_]]
    val maybeDate: Option[ZonedDateTime] = shape.getField(model, "maybeDate")
    inside(shape.getField(model, "maybeDate")) {
      case x: Option[ZonedDateTime] if x == Some(date) =>
    }

    // Seq[T]
    shape.tags shouldBe an[shape.Field[_]]
    val seq: Seq[String] = shape.getField(model, "tags")
    inside(shape.getField(model, "tags")) {
      case x: Seq[String] if x == tagsValue =>
    }
  }

  it should "find lift for basic types" in {
    FieldLift[Boolean]
    FieldLift[Int]
    FieldLift[Long]
    FieldLift[BigInt]
    FieldLift[Double]
    FieldLift[BigDecimal]
    FieldLift[String]
    FieldLift[ZonedDateTime]
    FieldLift[UUID]
    FieldLift[Json]
    FieldLift[ByteString]
  }

  it should "find lift for basic types inside Option" in {
    FieldLift[Option[Boolean]]
    FieldLift[Option[Int]]
    FieldLift[Option[Long]]
    FieldLift[Option[BigInt]]
    FieldLift[Option[Double]]
    FieldLift[Option[BigDecimal]]
    FieldLift[Option[String]]
    FieldLift[Option[ZonedDateTime]]
    FieldLift[Option[UUID]]
    FieldLift[Option[Json]]
    FieldLift[Option[ByteString]]

    FieldLift[Option[Option[String]]]
    FieldLift[Option[Seq[String]]]
  }

  it should "find lift for basic types inside Seq" in {
    FieldLift[Seq[Boolean]]
    FieldLift[Seq[Int]]
    FieldLift[Seq[Long]]
    FieldLift[Seq[BigInt]]
    FieldLift[Seq[Double]]
    FieldLift[Seq[BigDecimal]]
    FieldLift[Seq[String]]
    FieldLift[Seq[ZonedDateTime]]
    FieldLift[Seq[UUID]]
    FieldLift[Seq[Json]]
    FieldLift[Seq[ByteString]]

    FieldLift[Seq[Option[String]]]
    FieldLift[Seq[Seq[String]]]
  }


  case class Nickname(name: String)

  object NicknameShape
    extends Shape(Nickname.apply _)
      with IdeaTypeHint[Nickname] {

    implicit val name = field("name", _.name)

    def projection: Projection = name :-: SNil
  }

  case class User(a: Int, b: String, c: Boolean, d: Option[String], e: Option[String], nick: Nickname)

  object UserShape
    extends Shape(User.apply _)
      with IdeaTypeHint[User] {

    implicit val a = field("a", _.a)
    implicit val b = field("b", _.b)
    implicit val c = field("c2", _.c)
    implicit val d = field("d", _.d)
    implicit val e = field("e", _.e)
    implicit val n = sub("n", _.nick, NicknameShape)

    def projection: Projection = a :-: b :-: c :-: d :-: e :-: n :-: SNil
  }

  import io.circe.generic.auto._
  object UserCirceShape extends CirceShape[User]

  case class Rights(user: User, userRights: Seq[String])
  object RightsShape extends CirceShape[Rights]

  object TestDatabase extends DatabaseShape("test") {

    val users = table("users", UserShape)

    val usersJson = table("usersJson", UserCirceShape)

    val rights = table("rights", RightsShape)
  }

  it should "work with complex shapes" in {
    def toJsonString(reqlObject: ReqlObject): String = {
      import cats.instances.function._
      val renderer = new ByteStringRenderer(StandardCharsets.UTF_8)
      reqlObject.trampolinedRasterizer.rasterize(renderer).run.get.utf8String
    }

    import UserShape.ByNameGetter

    val user = User(123, "bcd", false, Some("def"), None, Nickname("user"))

    val i4: Int = user.get("a")
    i4 shouldBe 123

    val i5: Int = UserShape.getField(user, "a")
    i5 shouldBe 123

    val n1: Nickname = UserShape.getField(user, "n")
    n1 shouldBe Nickname("user")

    val m = UserShape.toMap(user)
    m shouldBe Map(
      "a" -> 123,
      "b" -> "bcd",
      "c2" -> false,
      "d" -> Some("def"),
      "e" -> None,
      "n" -> Nickname("user")
    )

    val reqlObj = UserShape.toReqlObject(user)
    toJsonString(reqlObj) shouldBe """{"e":null,"n":{"name":"user"},"a":123,"c2":false,"b":"bcd","d":"def"}"""

    val json = io.circe.parser.parse("""{"e":null,"n":{"name":"user"},"a":123,"c2":false,"b":"bcd","d":"def"}""").right.get
    val maybeUser = UserShape.fromJson(json)
    maybeUser shouldBe Right(user)

    val badJson = Json.fromJsonObject(JsonObject.fromMap(Map("l" -> Json.False)))
    val maybeUser2 = UserShape.fromJson(badJson)
    maybeUser2 shouldBe Left(ShapeDecodingError("Field 'a' is missing", badJson))

    val badJson2 = Json.fromJsonObject(JsonObject.fromMap(Map("a" -> Json.False)))
    val maybeUser3 = UserShape.fromJson(badJson2)
    maybeUser3 shouldBe Left(ShapeDecodingError("Field 'a': Not a number", Json.False))

    // Circe shape
    val reqlObj2 = UserCirceShape.toReqlObject(user)
    toJsonString(reqlObj2) shouldBe """{"a":123,"b":"bcd","c":false,"d":"def","e":null,"nick":{"name":"user"}}"""

    val circeJsonStr = """{"a":123,"b":"bcd","c":false,"d":"def","e":null,"nick":{"name":"user"}}"""
    val circeJson = io.circe.parser.parse(circeJsonStr).right.get
    val maybeUser4 = UserCirceShape.fromJson(circeJson)
    maybeUser4 shouldBe Right(user)

    val rightsSample = Rights(user, Seq("moderator", "manager", "viewer"))
    val rightsReqlObj = RightsShape.toReqlObject(rightsSample)
    toJsonString(rightsReqlObj) shouldBe """{"user":{"a":123,"b":"bcd","c":false,"d":"def","e":null,"nick":{"name":"user"}},"userRights":[2,["moderator","manager","viewer"]]}"""

    val t: ReqlTable[ReqlModel[User]] = TestDatabase.users.table()
  }

}
