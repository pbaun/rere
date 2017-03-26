package rere.driver

import akka.actor.{ActorSystem, Terminated}
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import rere.driver.pool.{ConnectionPool, ShutdownSuccessfullyDone}

import scala.concurrent.ExecutionContext
import scala.util.Random

class ManyModelsInsertionTest extends WordSpec with ScalaFutures with Matchers {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(100, Millis))

  "driver" should {
    "insert many models" in {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val poolSize = 1
      val pool = ConnectionPool.create(credentials, settings, "pool", poolSize)

      import rere.driver.runners.all._
      import rere.ql.queries.all._
      import rere.ql.shapes._
      import rere.ql.options.all._
      import io.circe.generic.auto._

      case class Abc(id: String, name: Option[String])
      object AbcShape extends CirceShape[Abc, String]

      object TestDatabase extends DatabaseShape("test") {
        implicit val abc = table("abc", AbcShape)
      }

      import TestDatabase.abc

      val model: Abc = Abc("123-a", Some("abc name " + Random.nextInt()))
      val model2: Abc = Abc("234-b", Some("bcd name " + Random.nextInt()))

      whenReady(abc.table().insertMany(
        r.expr(Seq(model, model2)),
        durability = Hard,
        returnChanges = DoReturnChanges,
        conflict = ResolveOnConflict[ReqlModel[Abc]]((x, a, b) => b)
      ).run(pool).future()) { result =>
        result.inserted shouldBe 0
        result.replaced shouldBe 2
        result.generatedKeys shouldBe None
        result.changes shouldBe an[Some[_]]
        result.changes.get should have size 2

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
