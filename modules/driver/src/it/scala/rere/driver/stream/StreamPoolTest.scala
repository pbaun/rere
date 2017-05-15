package rere.driver.stream

import java.time.ZonedDateTime

import akka.Done
import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import com.typesafe.config.ConfigFactory
import io.circe.JsonObject
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Minutes, Span}
import org.scalatest.{FlatSpec, Matchers}
import rere.driver.pool.ConnectionPool
import rere.driver.runners.all._
import rere.driver.{ConnectionSettings, Credentials}
import rere.ql.data.ChangefeedNotification
import rere.ql.queries.all._

import scala.concurrent.ExecutionContext

class StreamPoolTest extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(10, Minutes), interval = Span(100, Millis))

  behavior of "StreamPool"

  implicit val ec = ExecutionContext.Implicits.global
  val config = ConfigFactory.parseString("akka.loglevel=DEBUG")
  implicit val system = ActorSystem("rere", config)

  val credentials = Credentials("admin", "")
  //val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)
  val settings = ConnectionSettings("192.168.1.108", 28015, ConnectionSettings.noSslConnection)

  it should "connect and fetch stream" in {

    val start = System.currentTimeMillis()

    val pool = ConnectionPool.create(credentials, settings, "test", 4)

    val sinkDone = r.range(1000L).run(pool).drainTo(Sink.foreach {
      var first = true
      el: Long =>

        if (first) {
          println(s"first in ${System.currentTimeMillis() - start} ms")
          first = false
        }

        println(s"-- $el")
    })

    whenReady(sinkDone) { done =>
      println("sink done")

      println(s"in ${System.currentTimeMillis() - start} ms")

      done shouldBe Done
    }

    whenReady(pool.shutdown()) { done =>
      done shouldBe Done

      println("shutdown done")
    }
  }

  it should "connect and fetch atom" ignore {

    val start = System.currentTimeMillis()

    val pool = ConnectionPool.create(credentials, settings, "test", 4)

    val resultFuture = r.now().run(pool).future()

    whenReady(resultFuture) { result =>
      println("result done")
      println(s"in ${System.currentTimeMillis() - start} ms")
      println(result)

      result shouldBe an [ZonedDateTime]
    }

    whenReady(pool.shutdown()) { done =>
      done shouldBe Done

      println("shutdown done")
    }
  }

  it should "fail on database shutdown during request" ignore {

    val start = System.currentTimeMillis()

    val pool = ConnectionPool.create(credentials, settings, "test", 1)

    val sinkDone = r.table("tv_shows").changes().run(pool).drainTo(
      Sink.foreach[ChangefeedNotification[JsonObject]] {
        var first = true
        el =>

          if (first) {
            println(s"first in ${System.currentTimeMillis() - start} ms")
            first = false
          }

          println(s"-- $el")
      }
    )

    // Database shutdown or drop connection

    whenReady(sinkDone.failed) { ex =>
      println("sink done")

      println(s"in ${System.currentTimeMillis() - start} ms")

      ex shouldBe an [IllegalStateException]
      ex.getMessage shouldBe "Premature finish"
    }

    whenReady(pool.shutdown()) { done =>
      done shouldBe Done

      println("shutdown done")
    }
  }

}
