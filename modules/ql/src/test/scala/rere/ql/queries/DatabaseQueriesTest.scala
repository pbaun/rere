package rere.ql.queries

import org.scalatest.FlatSpec
import rere.ql.types._

class DatabaseQueriesTest extends FlatSpec with ReqlMatchers {

  import rere.ql.queries.all.r
  import rere.ql.queries.db._
  import rere.ql.queries.math._
  import rere.ql.queries.values._

  behavior of "DatabaseQueries"

  it should "provide .db operator on r" in {
    r.db("test") shouldBe subtypeOf [ReqlDatabase] and serializedTo("""[14,["test"]]""")
  }

  it should "provide .dbCreate operator on r" in {
    r.dbCreate("superheroes") shouldBe
      subtypeOf [ReqlDatabaseCreationResult] and serializedTo("""[57,["superheroes"]]""")

    r.dbCreate(r.expr("super").add("heroes")) shouldBe
      subtypeOf [ReqlDatabaseCreationResult] and serializedTo("""[57,[[24,["super","heroes"]]]]""")
  }

  it should "provide .dbDrop operator on r" in {
    r.dbDrop("superheroes") shouldBe
      subtypeOf [ReqlDatabaseDroppingResult] and serializedTo("""[58,["superheroes"]]""")

    r.dbDrop(r.expr("super").add("heroes")) shouldBe
      subtypeOf [ReqlDatabaseDroppingResult] and serializedTo("""[58,[[24,["super","heroes"]]]]""")
  }

  it should "provide .dbList operator on r" in {
    r.dbList() shouldBe subtypeOf [ReqlArray[ReqlString]] and serializedTo("""[59,[]]""")
  }
}
