package rere.ql.shapes

import org.scalatest.FlatSpec
import rere.ql.options.all._
import rere.ql.queries.ReqlMatchers
import rere.ql.types.{PrimaryKey, ReqlTable}

class DatabaseShapeTest extends FlatSpec with ReqlMatchers {

  import io.circe.generic.auto._

  case class Nickname(name: String)
  case class User(id: Long, nick: Nickname)
  object UserShape extends CirceShape[User, PrimaryKey.Int]

  case class Rights(user: User, userRights: Seq[String])
  object RightsShape extends CirceShape[Rights, PrimaryKey.String]

  object TestDatabase extends DatabaseShape("test") {
    val users = table("users", UserShape)
    val rights = table("rights", RightsShape, readMode = Majority, identifierFormat = UuidIdentifier)
  }

  behavior of "DatabaseShape"

  it should "allow to use unconfigured table descriptor" in {
    TestDatabase.users.table() shouldBe
      subtypeOf[ReqlTable[User, PrimaryKey.Int]] and
      serializedTo("""[15,[[14,["test"]],"users"]]""")
  }

  it should "allow to configure unconfigured table descriptor" in {
    TestDatabase.users.table(readMode = Majority) shouldBe
      subtypeOf[ReqlTable[User, PrimaryKey.Int]] and
      serializedTo("""[15,[[14,["test"]],"users"],{"read_mode":"majority"}]""")

    TestDatabase.users.table(identifierFormat = UuidIdentifier) shouldBe
      subtypeOf[ReqlTable[User, PrimaryKey.Int]] and
      serializedTo("""[15,[[14,["test"]],"users"],{"identifier_format":"uuid"}]""")

    TestDatabase.users.table(readMode = Outdated, identifierFormat = NameIdentifier) shouldBe
      subtypeOf[ReqlTable[User, PrimaryKey.Int]] and
      serializedTo("""[15,[[14,["test"]],"users"],{"read_mode":"outdated","identifier_format":"name"}]""")
  }

  it should "allow to use preconfigured table descriptor" in {
    TestDatabase.rights.table() shouldBe
      subtypeOf[ReqlTable[Rights, PrimaryKey.String]] and
      serializedTo("""[15,[[14,["test"]],"rights"],{"read_mode":"majority","identifier_format":"uuid"}]""")
  }

  it should "allow to override configuration of preconfigured table descriptor" in {
    TestDatabase.rights.table(readMode = Outdated) shouldBe
      subtypeOf[ReqlTable[Rights, PrimaryKey.String]] and
      serializedTo("""[15,[[14,["test"]],"rights"],{"read_mode":"outdated","identifier_format":"uuid"}]""")

    TestDatabase.rights.table(identifierFormat = NameIdentifier) shouldBe
      subtypeOf[ReqlTable[Rights, PrimaryKey.String]] and
      serializedTo("""[15,[[14,["test"]],"rights"],{"read_mode":"majority","identifier_format":"name"}]""")

    TestDatabase.rights.table(readMode = Outdated, identifierFormat = NameIdentifier) shouldBe
      subtypeOf[ReqlTable[Rights, PrimaryKey.String]] and
      serializedTo("""[15,[[14,["test"]],"rights"],{"read_mode":"outdated","identifier_format":"name"}]""")
  }

}
