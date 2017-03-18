package rere.driver

import java.util.UUID

import akka.actor.ActorSystem
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{Matchers, WordSpec}
import rere.driver.pool.ConnectionPool
import rere.ql.data.{Shard, TableConfig}

import scala.concurrent.ExecutionContext

class TableConfigFetchTest extends WordSpec with ScalaFutures with Matchers {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(100, Millis))

  "driver" should {
    "fetch table config" in {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val pool = ConnectionPool.create(credentials, settings, "pool", 1)
      import rere.driver.runners.all._
      import rere.ql.queries.all._

      whenReady(r.db("test").table("abc").config().run(pool).future()) { result =>
        result shouldBe TableConfig(
          id = UUID.fromString("f52e39dc-79d3-48c6-98f1-43eab399d449"),
          name = "abc",
          db = "test",
          primaryKey = "id",
          shards = List(Shard("MacBook_Pavel_local_hn5", List("MacBook_Pavel_local_hn5"), Nil)),
          indexes = List("area", "code", "code1", "code3", "name1"),
          writeAcks = "majority",
          durability = "hard"
        )
      }
    }
  }
}
