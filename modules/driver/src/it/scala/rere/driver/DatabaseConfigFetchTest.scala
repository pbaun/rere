package rere.driver

import java.util.UUID

import akka.actor.ActorSystem
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{Matchers, WordSpec}
import rere.driver.pool.ConnectionPool
import rere.ql.data.DatabaseConfig

import scala.concurrent.ExecutionContext

class DatabaseConfigFetchTest extends WordSpec with ScalaFutures with Matchers {

  implicit val defaultPatience =
     PatienceConfig(timeout = Span(15, Seconds), interval = Span(100, Millis))

  "driver" should {
    "fetch database config" in {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val pool = ConnectionPool.create(credentials, settings, "pool", 1)
      import rere.driver.runners.all._
      import rere.ql.queries.all._

      whenReady(r.db("test").config().run(pool).future()) { result =>
        result shouldBe DatabaseConfig(UUID.fromString("437f7ba9-ba8e-4613-a041-e7a0cb11dfdf"), "test")
      }
    }
  }
}