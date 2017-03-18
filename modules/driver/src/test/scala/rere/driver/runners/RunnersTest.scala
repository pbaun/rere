package rere.driver.runners

import java.time.ZonedDateTime

import akka.Done
import akka.stream.scaladsl.Sink
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Inside, Matchers}
import rere.driver.CaptorSugar
import rere.driver.pool.{AcquireConnection, ConnectionPool, ConnectionPoolIncomingMessages}
import rere.driver.protocol.{Atom, Stream}
import rere.driver.runners.ready.{FiniteStreamReadyToGo, InfiniteStreamReadyToGo, SingleValueReadyToGo}
import rere.ql.data.{ChangefeedNotification, ModificationResult}
import rere.ql.shapes.{CirceShape, DatabaseShape}
import rere.ql.types.ReqlJsonObject

import scala.concurrent.Future

class RunnersTest extends FlatSpec with Matchers with MockitoSugar with CaptorSugar with Inside {

  import rere.driver.runners.all._

  case class Author(id: String, name: String)
  case class Abc(id: String, name: Option[String])

  import io.circe.generic.auto._
  object AuthorsShape extends CirceShape[Author]
  object AbcShape extends CirceShape[Abc]

  object TestDatabase extends DatabaseShape("test") {
    val authors = table[Author]("authors", AuthorsShape)
    val abc = table[Abc]("abc", AbcShape)
  }


  behavior of "Runners object and implicit classes inside it"

  it should "allow to build single value selection query" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    r.now().run(pool)

    val q: SingleValueReadyToGo[ZonedDateTime] = r.now().run(pool)

    verifyZeroInteractions(pool)


    val f: Future[ZonedDateTime] = r.now().run(pool).future()
    val msgCaptor = captor(classOf[ConnectionPoolIncomingMessages])
    verify(pool).send(msgCaptor.capture())

    inside(msgCaptor.getValue) {
      case AcquireConnection(responseType, _, _) =>
        responseType shouldBe Atom
    }
    verifyNoMoreInteractions(pool)
  }

  it should "allow to build single json object selection query" in {
    import io.circe.JsonObject
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    r.table[ReqlJsonObject]("abc").get("uuid").run(pool)

    val q: SingleValueReadyToGo[JsonObject] = r.table[ReqlJsonObject]("abc").get("uuid").run(pool)

    verifyZeroInteractions(pool)


    val f: Future[JsonObject] = r.table[ReqlJsonObject]("abc").get("uuid").run(pool).future()
    val msgCaptor = captor(classOf[ConnectionPoolIncomingMessages])
    verify(pool).send(msgCaptor.capture())

    inside(msgCaptor.getValue) {
      case AcquireConnection(responseType, _, _) =>
        responseType shouldBe Atom
    }
    verifyNoMoreInteractions(pool)
  }

  it should "allow to build single model selection query" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    implicit val abcShape = AbcShape

    TestDatabase.abc.table().get("uuid").run(pool)

    val q: SingleValueReadyToGo[Abc] = TestDatabase.abc.table().get("uuid").run(pool)

    verifyZeroInteractions(pool)


    val f: Future[Abc] = TestDatabase.abc.table().get("uuid").run(pool).future()
    val msgCaptor = captor(classOf[ConnectionPoolIncomingMessages])
    verify(pool).send(msgCaptor.capture())

    inside(msgCaptor.getValue) {
      case AcquireConnection(responseType, _, _) =>
        responseType shouldBe Atom
    }
    verifyNoMoreInteractions(pool)
  }

  it should "allow to build finite stream selection query" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    r.range(0, 1000).run(pool)

    val q: FiniteStreamReadyToGo[Long, _] = r.range(0, 1000).run(pool)

    verifyZeroInteractions(pool)


    val sink = Sink.seq[Long]
    val (futureMat: Future[Future[Seq[Long]]], futureDone: Future[Done]) =
      r.range(0, 1000).run(pool).drainTo(sink)
    val msgCaptor = captor(classOf[ConnectionPoolIncomingMessages])
    verify(pool).send(msgCaptor.capture())

    inside(msgCaptor.getValue) {
      case AcquireConnection(responseType, _, _) =>
        responseType shouldBe Stream
    }
    verifyNoMoreInteractions(pool)
  }

  it should "allow to build infinite stream selection query" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    r.range().run(pool)

    val q: InfiniteStreamReadyToGo[Long, _] = r.range().run(pool)

    verifyZeroInteractions(pool)


    val sink = Sink.seq[Long]
    val (futureMat: Future[Future[Seq[Long]]], futureDone: Future[Done]) =
      r.range().run(pool).drainTo(sink)
    val msgCaptor = captor(classOf[ConnectionPoolIncomingMessages])
    verify(pool).send(msgCaptor.capture())

    inside(msgCaptor.getValue) {
      case AcquireConnection(responseType, _, _) =>
        responseType shouldBe Stream
    }
    verifyNoMoreInteractions(pool)
  }

  it should "allow to build changefeed selection query" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    implicit val abcShape = AbcShape

    TestDatabase.abc.table().get("uuid").changes().run(pool)

    val q: InfiniteStreamReadyToGo[ChangefeedNotification[Abc], _] =
      TestDatabase.abc.table().get("uuid").changes().run(pool)

    verifyZeroInteractions(pool)


    val sink = Sink.seq[ChangefeedNotification[Abc]]
    val (futureMat: Future[Future[Seq[ChangefeedNotification[Abc]]]], futureDone: Future[Done]) =
      TestDatabase.abc.table().get("uuid").changes().run(pool).drainTo(sink)
    val msgCaptor = captor(classOf[ConnectionPoolIncomingMessages])
    verify(pool).send(msgCaptor.capture())

    inside(msgCaptor.getValue) {
      case AcquireConnection(responseType, _, _) =>
        responseType shouldBe Stream
    }
    verifyNoMoreInteractions(pool)
  }

  it should "allow to build single model insertion query" in {
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    implicit val abcShape = AbcShape
    val model: Abc = Abc("123", Some("abc name"))

    TestDatabase.abc.table().insert(model).run(pool)

    val q: SingleValueReadyToGo[ModificationResult[Abc]] =
      TestDatabase.abc.table().insert(model).run(pool)

    verifyZeroInteractions(pool)


    val f: Future[ModificationResult[Abc]] = TestDatabase.abc.table().insert(model).run(pool).future()
    val msgCaptor = captor(classOf[ConnectionPoolIncomingMessages])
    verify(pool).send(msgCaptor.capture())

    inside(msgCaptor.getValue) {
      case AcquireConnection(responseType, _, _) =>
        responseType shouldBe Atom
    }
    verifyNoMoreInteractions(pool)
  }

  it should "allow to build single json object insertion query" in {
    import io.circe.{Json, JsonObject}
    import rere.ql.queries.all._

    val pool = mock[ConnectionPool]

    val obj = JsonObject.fromMap(Map("field" -> Json.fromString("data")))

    r.db("test").table[ReqlJsonObject]("abc").insert(obj).run(pool)

    val q: SingleValueReadyToGo[ModificationResult[JsonObject]] =
      r.db("test").table[ReqlJsonObject]("abc").insert(obj).run(pool)

    verifyZeroInteractions(pool)


    val f: Future[ModificationResult[JsonObject]] =
      r.db("test").table[ReqlJsonObject]("abc").insert(obj).run(pool).future()
    val msgCaptor = captor(classOf[ConnectionPoolIncomingMessages])
    verify(pool).send(msgCaptor.capture())

    inside(msgCaptor.getValue) {
      case AcquireConnection(responseType, _, _) =>
        responseType shouldBe Atom
    }
    verifyNoMoreInteractions(pool)
  }

}
