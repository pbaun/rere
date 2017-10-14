package rere.driver.workers

import akka.Done
import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.scaladsl.{GraphDSL, RunnableGraph}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import akka.testkit.TestKit
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import rere.driver.connection.LogicalConnectionProtocol.{RawResponse, RenderedCommand}
import rere.driver.exceptions.{ReqlDriverError, ReqlQueryShutdownException}
import rere.ql.options.Options
import rere.ql.queries.all._
import rere.ql.types.ReqlString
import rere.ql.wire.ReqlDecoder

import scala.concurrent.duration._

class StreamQueryWorkerTest
  extends TestKit(ActorSystem("StreamQueryWorkerTest", ConfigFactory.parseString(StreamQueryWorkerTest.config)))
  with FlatSpecLike
  with BeforeAndAfterAll
  with ScalaFutures
  with Matchers {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
    super.afterAll()
  }

  trait mocks {
    implicit val mat = ActorMaterializer(
      ActorMaterializerSettings.create(system).withInputBuffer(16, 16)
    )

    val expr = r.expr("abc")
    val runOptions = Options.empty
    val reqlDecoder = ReqlDecoder[String]
    val logger = Logging(system, "stream-worker-test")

    val graph = RunnableGraph.fromGraph(GraphDSL.create(
      TestSource.probe[RawResponse],
      TestSink.probe[RenderedCommand],
      TestSink.probe[String],
      new StreamQueryWorker[ReqlString, String](expr, runOptions, reqlDecoder, logger)) {
      (dbResponses, commands, out, worker) => (dbResponses, commands, out, worker)
    } { implicit builder =>
      (dbResponses, commands, out, worker) =>

        import GraphDSL.Implicits._

        out <~ worker.out
               worker.commands    ~> commands
               worker.dbResponses <~ dbResponses

        ClosedShape
    })

    val (dbResponsesSource, commandsSink, outSink, workerDone) = graph.run()
  }

  behavior of "StreamQueryWorker"

  it should "send start command and read elements from responses" in new mocks {
    dbResponsesSource.ensureSubscription()
    commandsSink.ensureSubscription()
    outSink.ensureSubscription()

    commandsSink.request(1L)

    val startCommand = commandsSink.expectNext()
    startCommand.body shouldBe ByteString("""[1,"abc",{}]""")

    dbResponsesSource.sendNext(RawResponse(ByteString(
      """{"t":3,"r":["abc","bcd"]}"""
    )))

    outSink.request(4L)
    outSink.expectNext("abc")
    outSink.expectNext("bcd")

    commandsSink.request(1L)
    val moreCommand = commandsSink.expectNext()
    moreCommand.body shouldBe ByteString("[2]")

    dbResponsesSource.sendNext(RawResponse(ByteString(
      """{"t":2,"r":["cde"]}"""
    )))

    outSink.expectNext("cde")
    outSink.expectComplete()

    commandsSink.expectComplete()
    dbResponsesSource.expectCancellation()

    whenReady(workerDone) { done =>
      done shouldBe Done
    }
  }

  it should "not request more elements before out will ask" in new mocks {
    dbResponsesSource.ensureSubscription()
    commandsSink.ensureSubscription()
    outSink.ensureSubscription()

    commandsSink.request(1L)

    val startCommand = commandsSink.expectNext()
    startCommand.body shouldBe ByteString("""[1,"abc",{}]""")

    dbResponsesSource.sendNext(RawResponse(ByteString(
      """{"t":3,"r":["abc","bcd"]}"""
    )))

    outSink.request(1L)
    outSink.expectNext("abc")
    outSink.expectNoMessage(200.millis)

    outSink.request(1L)
    outSink.expectNext("bcd")
    outSink.expectNoMessage(200.millis)

    commandsSink.request(1L)
    commandsSink.expectNoMessage(200.millis)

    outSink.request(1L)

    val moreCommand = commandsSink.expectNext()
    moreCommand.body shouldBe ByteString("[2]")

    dbResponsesSource.sendNext(RawResponse(ByteString(
      """{"t":2,"r":["cde"]}"""
    )))

    outSink.expectNext("cde")
    outSink.expectComplete()

    commandsSink.expectComplete()
    dbResponsesSource.expectCancellation()

    whenReady(workerDone) { done =>
      done shouldBe Done
    }
  }

  it should "react on shutdown signal right after start" in new mocks {
    dbResponsesSource.ensureSubscription()
    commandsSink.ensureSubscription()
    outSink.ensureSubscription()

    dbResponsesSource.sendError(new ReqlQueryShutdownException)

    commandsSink.expectError() shouldBe an [ReqlQueryShutdownException]
    outSink.expectError() shouldBe an [ReqlQueryShutdownException]

    whenReady(workerDone.failed) { ex =>
      ex shouldBe an [ReqlQueryShutdownException]
    }
  }

  it should "react on shutdown signal after command pulled" in new mocks {
    dbResponsesSource.ensureSubscription()
    commandsSink.ensureSubscription()
    outSink.ensureSubscription()

    commandsSink.request(1L)
    dbResponsesSource.sendError(new ReqlQueryShutdownException)

    val startCommand = commandsSink.expectNext()
    startCommand.body shouldBe ByteString("""[1,"abc",{}]""")

    commandsSink.expectError() shouldBe an [ReqlQueryShutdownException]
    outSink.expectError() shouldBe an [ReqlQueryShutdownException]

    whenReady(workerDone.failed) { ex =>
      ex shouldBe an [ReqlQueryShutdownException]
    }
  }

  it should "react on connection lost signal right after start" in new mocks {
    dbResponsesSource.ensureSubscription()
    commandsSink.ensureSubscription()
    outSink.ensureSubscription()

    dbResponsesSource.sendError(new ReqlDriverError("Server unreachable"))

    commandsSink.expectError() shouldBe an [ReqlDriverError]
    outSink.expectError() shouldBe an [ReqlDriverError]

    whenReady(workerDone.failed) { ex =>
      ex shouldBe an [ReqlDriverError]
    }
  }

}

object StreamQueryWorkerTest {

  val config = """
    akka {
      loglevel = "WARNING"
    }
  """
}
