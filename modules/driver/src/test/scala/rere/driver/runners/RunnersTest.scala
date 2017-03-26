package rere.driver.runners

import java.time.ZonedDateTime
import java.util.UUID

import akka.Done
import akka.stream.scaladsl.Sink
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Inside, Matchers}
import rere.driver.pool.{AcquireConnection, ConnectionPool, ConnectionPoolIncomingMessages}
import rere.driver.protocol.{Atom, Stream}
import rere.driver.runners.ready.{FiniteStreamReadyToGo, InfiniteStreamReadyToGo, SingleValueReadyToGo}
import rere.ql.data.{ChangefeedNotification, ModificationResult}
import rere.ql.shapes.{CirceShape, DatabaseShape}
import rere.ql.types.ReqlJsonObject

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
    val f: Future[ZonedDateTime] = r.now().run(pool).future()

    pool.send _ verify where { msg: ConnectionPoolIncomingMessages =>
      inside(msg) {
        case AcquireConnection(responseType, _, _) =>
          responseType shouldBe Atom
          true
      }
    }
  }

  it should "not actually run single json object selection query after .run call" in {
    import io.circe.JsonObject
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    r.table[ReqlJsonObject, UUID]("abc").get("uuid").run(pool)
    val q: SingleValueReadyToGo[JsonObject] = r.table[ReqlJsonObject, UUID]("abc").get("uuid").run(pool)
  }

  it should "allow to run single json object selection query" in {
    import io.circe.JsonObject
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]
    val f: Future[JsonObject] = r.table[ReqlJsonObject, UUID]("abc").get("uuid").run(pool).future()

    pool.send _ verify where { msg: ConnectionPoolIncomingMessages =>
      inside(msg) {
        case AcquireConnection(responseType, _, _) =>
          responseType shouldBe Atom
          true
      }
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

    val f: Future[Abc] = TestDatabase.abc.table().get("uuid").run(pool).future()

    pool.send _ verify where { msg: ConnectionPoolIncomingMessages =>
      inside(msg) {
        case AcquireConnection(responseType, _, _) =>
          responseType shouldBe Atom
          true
      }
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
    val (futureMat: Future[Future[Seq[Long]]], futureDone: Future[Done]) =
      r.range(0, 1000).run(pool).drainTo(sink)

    pool.send _ verify where { msg: ConnectionPoolIncomingMessages =>
      inside(msg) {
        case AcquireConnection(responseType, _, _) =>
          responseType shouldBe Stream
          true
      }
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
    val (futureMat: Future[Future[Seq[Long]]], futureDone: Future[Done]) =
      r.range().run(pool).drainTo(sink)

    pool.send _ verify where { msg: ConnectionPoolIncomingMessages =>
      inside(msg) {
        case AcquireConnection(responseType, _, _) =>
          responseType shouldBe Stream
          true
      }
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
    val (futureMat: Future[Future[Seq[ChangefeedNotification[Abc]]]], futureDone: Future[Done]) =
      TestDatabase.abc.table().get("uuid").changes().run(pool).drainTo(sink)

    pool.send _ verify where { msg: ConnectionPoolIncomingMessages =>
      inside(msg) {
        case AcquireConnection(responseType, _, _) =>
          responseType shouldBe Stream
          true
      }
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

    val f: Future[ModificationResult[Abc, String]] = TestDatabase.abc.table().insert(model).run(pool).future()

    pool.send _ verify where { msg: ConnectionPoolIncomingMessages =>
      inside(msg) {
        case AcquireConnection(responseType, _, _) =>
          responseType shouldBe Atom
          true
      }
    }
  }

  it should "not actually run single json object insertion query after .run call" in {
    import io.circe.{Json, JsonObject}
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    val obj = JsonObject.fromMap(Map("field" -> Json.fromString("data")))

    r.db("test").table[ReqlJsonObject, UUID]("abc").insert(obj).run(pool)

    val q: SingleValueReadyToGo[ModificationResult[JsonObject, UUID]] =
      r.db("test").table[ReqlJsonObject, UUID]("abc").insert(obj).run(pool)
  }

  it should "allow to run single json object insertion query" in {
    import io.circe.{Json, JsonObject}
    import rere.ql.queries.all._

    val pool = stub[ConnectionPool]

    val obj = JsonObject.fromMap(Map("field" -> Json.fromString("data")))

    val f: Future[ModificationResult[JsonObject, UUID]] =
      r.db("test").table[ReqlJsonObject, UUID]("abc").insert(obj).run(pool).future()

    pool.send _ verify where { msg: ConnectionPoolIncomingMessages =>
      inside(msg) {
        case AcquireConnection(responseType, _, _) =>
          responseType shouldBe Atom
          true
      }
    }
  }

}
