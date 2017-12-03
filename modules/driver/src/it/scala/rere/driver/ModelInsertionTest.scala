package rere.driver

import akka.actor.{ActorSystem, Terminated}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{Matchers, WordSpec}
import rere.driver.pool.ConnectionPool
import rere.ql.types.PrimaryKey

import scala.concurrent.ExecutionContext
import scala.util.Random

class ModelInsertionTest extends WordSpec with ScalaFutures with Matchers {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(100, Millis))

  "driver" should {
    "insert model" in {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val poolSize = 1
      val pool = ConnectionPool.create(credentials, settings, "pool", poolSize)

      import io.circe.generic.auto._
      import rere.driver.runners.all._
      import rere.ql.queries.all._
      import rere.ql.shapes._

      case class Abc(id: String, name: Option[String])
      object AbcShape extends CirceShape[Abc, PrimaryKey.String]

      object TestDatabase extends DatabaseShape("test") {
        implicit val abc = table("abc", AbcShape)
      }

      import TestDatabase.abc

      val model: Abc = Abc(s"123_${Random.nextInt}", Some("abc name"))

      whenReady(abc.table().insert(model).run(pool).future()) { result =>
        result.inserted shouldBe 1
        result.generatedKeys shouldBe None

        whenReady(pool.shutdown()) { shutdownResult =>
          shutdownResult.queriesStarted shouldBe 1L
          shutdownResult.connectionsTurnedOff shouldBe poolSize

          whenReady(system.terminate()) { terminationResult =>
            terminationResult shouldBe an[Terminated]
          }
        }
      }
    }
  }
}
