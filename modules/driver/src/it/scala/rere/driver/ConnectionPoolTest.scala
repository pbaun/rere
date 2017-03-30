package rere.driver

import akka.Done
import akka.actor.{ActorSystem, Terminated}
import akka.stream.ThrottleMode
import akka.stream.scaladsl.{Flow, Sink}
import org.reactivestreams.{Subscriber, Subscription}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{Inside, Matchers, WordSpec}
import rere.driver.exceptions.{ReqlAuthError, ReqlDriverError}
import rere.driver.pool.{ConnectionPool, ShutdownSuccessfullyDone}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class ConnectionPoolTest extends WordSpec with Matchers with Inside {

  trait DefaultPatience extends ScalaFutures {
    implicit val patience =
      PatienceConfig(timeout = Span(15, Seconds), interval = Span(100, Millis))
  }

  trait ExtraPatience extends ScalaFutures {
    implicit val patience =
      PatienceConfig(timeout = Span(300, Seconds), interval = Span(500, Millis))
  }

  "connection pool" should {
    "start, work and shutdown" ignore new DefaultPatience {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val poolSize = 1
      val pool = ConnectionPool.create(credentials, settings, "pool", poolSize)

      import rere.driver.runners.all._
      import rere.ql.queries.all._

      val f = r.expr(3).add(r.expr(2)).run(pool).future()
      whenReady(f) { result =>
        result shouldBe 5

        val shutdownF = pool.shutdown()
        whenReady(shutdownF) { shutdownResult =>
          shutdownResult shouldBe ShutdownSuccessfullyDone(1, poolSize)

          whenReady(system.terminate()) { terminationResult =>
            terminationResult shouldBe an[Terminated]
          }
        }
      }
    }

    "fail if pool does not have ready connection" ignore new DefaultPatience {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val poolSize = 0
      val pool = ConnectionPool.create(credentials, settings, "pool", poolSize)

      import rere.driver.runners.all._
      import rere.ql.queries.all._

      val f = r.expr(3).add(r.expr(2)).run(pool).future()
      whenReady(f.failed) { ex =>
        ex shouldBe an[ReqlDriverError]
        ex.getMessage shouldBe "No connections left in the pool."

        val shutdownF = pool.shutdown()
        whenReady(shutdownF) { shutdownResult =>
          shutdownResult shouldBe ShutdownSuccessfullyDone(0, poolSize)

          whenReady(system.terminate()) { terminationResult =>
            terminationResult shouldBe an[Terminated]
          }
        }
      }
    }

    "crash if user name is't valid" ignore new DefaultPatience {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin2", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val poolSize = 8
      val pool = ConnectionPool.create(credentials, settings, "pool", 8)

      import rere.driver.runners.all._
      import rere.ql.queries.all._

      val f = r.expr(3).add(r.expr(2)).run(pool).future()

      whenReady(f.failed) { ex =>
        ex shouldBe a [ReqlAuthError]
        ex.getMessage shouldBe "Unknown user"

        val shutdownF = pool.shutdown()
        whenReady(shutdownF) { shutdownResult =>
          shutdownResult shouldBe ShutdownSuccessfullyDone(1, poolSize)

          whenReady(system.terminate()) { terminationResult =>
            terminationResult shouldBe an[Terminated]
          }
        }
      }
    }


    "correctly stop when finite stream competes" ignore new ExtraPatience {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)

      val poolSize = 1
      val pool = ConnectionPool.create(credentials, settings, "pool", poolSize)

      import rere.driver.runners.all._
      import rere.ql.queries.all._

      @volatile var lastSeenElement: Long = 0L
      @volatile var onCompleteCalled: Boolean = false

      val slowSink = Flow[Long].throttle(10, 100.millis, 5, ThrottleMode.Shaping).map({ number =>
        println(s"### onNext $number ${System.currentTimeMillis()}")
        lastSeenElement = number
      }).to(Sink.onComplete({ _ =>
        println(s"### onComplete")
        onCompleteCalled = true
      }))

      val customSink = Sink.fromSubscriber(new Subscriber[Long] {
        var sub: Subscription = _

        override def onSubscribe(s: Subscription): Unit = {
          sub = s
          sub.request(24)
        }

        override def onError(t: Throwable): Unit = {
          println(s"### onError $t")
          t.printStackTrace()
        }

        override def onComplete(): Unit = {
          println(s"### onComplete")
          onCompleteCalled = true
        }

        override def onNext(number: Long): Unit = {
          println(s"### onNext $number ${System.currentTimeMillis()}")
          //Thread.sleep(100L)
          lastSeenElement = number
          sub.request(1)
        }
      })

      val slowCustomSink = Flow[Long].throttle(10, 100.millis, 5, ThrottleMode.Shaping).to(customSink)

      val untilValue = 1000L
      val (sinkMat, doneF) = r.range(0, untilValue).run(pool).drainTo(slowSink)

      whenReady(doneF) { res =>
        res shouldBe akka.Done
        lastSeenElement shouldBe (untilValue - 1)
        onCompleteCalled shouldBe true

        val shutdownF = pool.shutdown()
        whenReady(shutdownF) { shutdownResult =>
          shutdownResult shouldBe ShutdownSuccessfullyDone(1L, poolSize)

          whenReady(system.terminate()) { terminationResult =>
            terminationResult shouldBe an[Terminated]
          }
        }
      }
    }

    "react to connection lost" in new ExtraPatience {
      implicit val ec = ExecutionContext.global
      implicit val system = ActorSystem("rere")
      val credentials = Credentials("admin", "")
      //val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)
      val settings = ConnectionSettings("192.168.1.108", 28015, ConnectionSettings.noSslConnection)

      val poolSize = 8
      val pool = ConnectionPool.create(credentials, settings, "pool", poolSize)

      import rere.driver.runners.all._
      import rere.ql.queries.all._

      /*val slowSink = Flow[Long].throttle(1, 500.millis, 5, ThrottleMode.Shaping).toMat(Sink.foreach { number =>
        println(s"@@Sink: $number ${System.currentTimeMillis()}")
      })(Keep.right)*/

      @volatile var completionResult: Try[Done] = _
      val slowSink = Flow[Long]
        .throttle(1, 500.millis, 5, ThrottleMode.Shaping)
        .map { number =>
          println(s"@@Sink: $number ${System.currentTimeMillis()}")
        }.to(Sink.onComplete { t =>
          completionResult = t
        })


      val (sinkMatF, doneF) = r.range(0, 1000).run(pool).drainTo(slowSink)

      // shutdown server or network adapter

      whenReady(doneF.failed) { ex =>
        //TODO: here can be 3 types of error:
        // 1. akka.stream.StreamTcpException: Connection failed - if server unreachable
        // 2. ReqlDriverError: Connection closed by server - if server turned off during query execution
        // 3. ReqlDriverError: Heartbeat check failed - if network interface turned off during query execution
        ex shouldBe an [ReqlDriverError]
        ex.getMessage should (startWith("Server unreachable") or startWith("Heartbeat check failed"))

        whenReady(sinkMatF) { _ =>
          inside(completionResult) {
            case Failure(matEx) =>
              matEx shouldBe an [ReqlDriverError]
              matEx.getMessage should (startWith("Server unreachable") or startWith("Heartbeat check failed"))
          }

          val shutdownF = pool.shutdown()
          whenReady(shutdownF) { shutdownResult =>
            inside(shutdownResult) {
              case ShutdownSuccessfullyDone(queriesExecuted, connectionsTurnedOff) =>
                queriesExecuted shouldBe 1
                connectionsTurnedOff should be >= poolSize
                connectionsTurnedOff should be <= poolSize * 3

                whenReady(system.terminate()) { terminationResult =>
                  terminationResult shouldBe an[Terminated]
                }
            }
          }
        }
      }
    }
  }

}
