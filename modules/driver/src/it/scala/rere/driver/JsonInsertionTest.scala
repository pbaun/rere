package rere.driver

import java.util.UUID

import akka.actor.{ActorSystem, Terminated}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{Matchers, WordSpec}
import rere.driver.pool.{ConnectionPool, ShutdownSuccessfullyDone}
import rere.ql.types.ReqlJsonObject

import scala.concurrent.ExecutionContext

class JsonInsertionTest extends WordSpec with ScalaFutures with Matchers {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(100, Millis))

  "driver" should {
    "insert json" in {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val poolSize = 1
      val pool = ConnectionPool.create(credentials, settings, "pool", poolSize)

      import io.circe.{Json, JsonObject}
      import rere.driver.runners.all._
      import rere.ql.queries.all._

      val model = JsonObject.fromMap(Map("field" -> Json.fromString("data")))

      whenReady(r.db("test").table[ReqlJsonObject, UUID]("abc").insert(model).run(pool).future()) { result =>
        result.inserted shouldBe 1
        result.generatedKeys shouldBe an[Some[Seq[String]]]
        result.generatedKeys.get should have size 1

        whenReady(pool.shutdown()) { shutdownResult =>
          shutdownResult shouldBe ShutdownSuccessfullyDone(1L, poolSize)

          whenReady(system.terminate()) { terminationResult =>
            terminationResult shouldBe an[Terminated]
          }
        }
      }
    }
  }
}
