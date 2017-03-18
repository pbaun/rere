package rere.driver

import java.time.{LocalDateTime, ZoneOffset}

import akka.actor.{ActorSystem, Terminated}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{Matchers, WordSpec}
import rere.driver.pool.{ConnectionPool, ShutdownSuccessfullyDone}

import scala.concurrent.ExecutionContext

class TimeFetchTest extends WordSpec with ScalaFutures with Matchers {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(100, Millis))

  "driver" should {
    "fetch time" in {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val poolSize = 1
      val pool = ConnectionPool.create(credentials, settings, "pool", poolSize)
      import rere.driver.runners.all._
      import rere.ql.queries.all._

      val utcOffset = ZoneOffset.UTC
      val now = LocalDateTime.now(utcOffset)

      whenReady(r.now().run(pool).future()) { result =>
        Math.abs(
          result.toLocalDateTime.toInstant(utcOffset).getEpochSecond - now.toInstant(utcOffset).getEpochSecond
        ) should be < 10L

        Math.abs(
          result.toInstant.getEpochSecond - now.toInstant(utcOffset).getEpochSecond
        ) should be < 10L

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
