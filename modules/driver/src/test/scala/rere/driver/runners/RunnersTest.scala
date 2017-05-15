package rere.driver.runners

import java.time.ZonedDateTime

import akka.stream.scaladsl.Sink
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Inside, Matchers}
import rere.driver.pool.ConnectionPool
import rere.driver.runners.ready._
import rere.ql.data.{ChangefeedNotification, ModificationResult}
import rere.ql.shapes.{CirceShape, DatabaseShape}
import rere.ql.types.{ReqlChangefeedNotification, ReqlFiniteStream, ReqlInfiniteStream, ReqlInteger}

import scala.concurrent.Future

class RunnersTest extends FlatSpec with Matchers with MockFactory with Inside {

  import rere.driver.runners.all._

  case class Author(id: String, name: String)
  case class Abc(id: String, name: Option[String])

  import io.circe.generic.auto._
  object AuthorsShape extends CirceShape[Author, String]
  object AbcShape extends CirceShape[Abc, String]

  object TestDatabase extends DatabaseShape("test") {
    val authors = table("authors", AuthorsShape)
    val abc = table("abc", AbcShape)
  }


  behavior of "Runners object and implicit classes inside it"

  it should "not actually run atom query after .run call" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    r.now().run(pool)
    val q: SingleValueReadyToGo[ZonedDateTime] = r.now().run(pool)
  }

  it should "allow to run single value selection query" in {
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]
    val q = r.now()
    val f: Future[ZonedDateTime] = q.run(pool).future()

    pool.runAtom _ verify where { case (expr, options, decoder) =>
      expr shouldBe q
      options.isEmpty shouldBe true
      true
    }
  }

  it should "not actually run single json object selection query after .run call" in {
    import io.circe.JsonObject
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    r.table[JsonObject, String]("abc").get("uuid").run(pool)
    val q: SingleValueReadyToGo[JsonObject] = r.table[JsonObject, String]("abc").get("uuid").run(pool)
  }

  it should "allow to run single json object selection query" in {
    import io.circe.JsonObject
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]
    val q = r.table[JsonObject, String]("abc").get("uuid")
    val f: Future[JsonObject] = q.run(pool).future()

    pool.runAtom _ verify where { case (expr, options, decoder) =>
      expr shouldBe q
      options.isEmpty shouldBe true
      true
    }
  }

  it should "not actually run single model selection query after .run call" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    implicit val abcShape = AbcShape

    TestDatabase.abc.table().get("uuid").run(pool)
    val q: SingleValueReadyToGo[Abc] = TestDatabase.abc.table().get("uuid").run(pool)
  }

  it should "allow to run single model selection query" in {
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]

    implicit val abcShape = AbcShape

    val q = TestDatabase.abc.table().get("uuid")
    val f: Future[Abc] = q.run(pool).future()

    pool.runAtom _ verify where { case (expr, options, decoder) =>
      expr shouldBe q
      options.isEmpty shouldBe true
      true
    }
  }

  it should "not actually run finite stream selection query after .run call" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    r.range(0, 1000).run(pool)
    val q: FiniteStreamReadyToGo[Long, _] = r.range(0, 1000).run(pool)
  }

  it should "allow to run finite stream selection query" in {
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]

    val sink = Sink.seq[Long]
    val q = r.range(0, 1000)
    val futureMat: Future[Seq[Long]] = q.run(pool).drainTo(sink)

    pool.runStream[ReqlFiniteStream[ReqlInteger], Long, Future[Seq[Long]]] _ verify
      where {
        case (expr, options, decoder, runSink) =>
          expr shouldBe q
          options.isEmpty shouldBe true
          runSink shouldBe sink
          true
      }
  }

  it should "not actually run infinite stream selection query after .run call" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    r.range().run(pool)
    val q: InfiniteStreamReadyToGo[Long, _] = r.range().run(pool)
  }

  it should "allow to run infinite stream selection query" in {
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]

    val sink = Sink.seq[Long]
    val q = r.range()
    val futureMat: Future[Seq[Long]] = q.run(pool).drainTo(sink)

    pool.runStream[ReqlInfiniteStream[ReqlInteger], Long, Future[Seq[Long]]] _ verify
      where {
        case (expr, options, decoder, runSink) =>
          expr shouldBe q
          options.isEmpty shouldBe true
          runSink shouldBe sink
          true
      }
  }

  it should "not actually run changefeed selection query after .run call" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    implicit val abcShape = AbcShape

    TestDatabase.abc.table().get("uuid").changes().run(pool)

    val q: InfiniteStreamReadyToGo[ChangefeedNotification[Abc], _] =
      TestDatabase.abc.table().get("uuid").changes().run(pool)
  }

  it should "allow to run changefeed selection query" in {
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]

    implicit val abcShape = AbcShape

    val sink = Sink.seq[ChangefeedNotification[Abc]]
    val q = TestDatabase.abc.table().get("uuid").changes()
    val futureMat: Future[Seq[ChangefeedNotification[Abc]]] = q.run(pool).drainTo(sink)

    pool.runStream[ReqlInfiniteStream[ReqlChangefeedNotification[Abc]], ChangefeedNotification[Abc], Future[Seq[Long]]] _ verify
      where {
        case (expr, options, decoder, runSink) =>
          expr shouldBe q
          options.isEmpty shouldBe true
          runSink shouldBe sink
          true
      }
  }

  it should "not actually run single model insertion query after .run call" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    implicit val abcShape = AbcShape
    val model: Abc = Abc("123", Some("abc name"))

    TestDatabase.abc.table().insert(model).run(pool)

    val q: SingleValueReadyToGo[ModificationResult[Abc, String]] =
      TestDatabase.abc.table().insert(model).run(pool)
  }

  it should "allow to run single model insertion query" in {
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]

    implicit val abcShape = AbcShape
    val model: Abc = Abc("123", Some("abc name"))

    val q = TestDatabase.abc.table().insert(model)
    val f: Future[ModificationResult[Abc, String]] = q.run(pool).future()

    pool.runAtom _ verify where { case (expr, options, decoder) =>
      expr shouldBe q
      options.isEmpty shouldBe true
      true
    }
  }

  it should "not actually run single json object insertion query after .run call" in {
    import io.circe.{Json, JsonObject}
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    val obj = JsonObject.fromMap(Map("field" -> Json.fromString("data")))

    r.db("test").table[JsonObject, String]("abc").insert(obj).run(pool)

    val q: SingleValueReadyToGo[ModificationResult[JsonObject, String]] =
      r.db("test").table[JsonObject, String]("abc").insert(obj).run(pool)
  }

  it should "allow to run single json object insertion query" in {
    import io.circe.{Json, JsonObject}
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]

    val obj = JsonObject.fromMap(Map("field" -> Json.fromString("data")))

    val q = r.db("test").table[JsonObject, String]("abc").insert(obj)
    val f: Future[ModificationResult[JsonObject, String]] = q.run(pool).future()

    pool.runAtom _ verify where { case (expr, options, decoder) =>
      expr shouldBe q
      options.isEmpty shouldBe true
      true
    }
  }

}
