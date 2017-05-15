package rere.driver.connection

import java.util.concurrent.ConcurrentHashMap

import akka.Done
import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, SourceQueueWithComplete}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, ClosedShape, QueueOfferResult}
import akka.testkit.TestKit
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpecLike, Matchers}
import rere.driver.connection.LogicalConnectionProtocol._

import scala.concurrent.Promise

class LogicalConnectionStageTest
  extends TestKit(ActorSystem("LogicalConnectionStageTest", ConfigFactory.parseString(LogicalConnectionStageTest.config)))
  with FlatSpecLike
  with ScalaFutures
  with Matchers
  with MockFactory {

  trait mocks {
    implicit val mat = ActorMaterializer(
      ActorMaterializerSettings.create(system).withInputBuffer(16, 16)
    )

    val routingTable = new ConcurrentHashMap[Long, SourceQueueWithComplete[RawResponse]]()
    val logger = Logging(system, "connection-test")

    val graph = RunnableGraph.fromGraph(GraphDSL.create(
      TestSource.probe[RenderedCommandWithToken],
      TestSource.probe[PrepareForShutdown.type],
      TestSource.probe[ReqlFrame],
      TestSink.probe[ReqlFrame],
      new LogicalConnectionStage(routingTable, logger)) {
      (commands, shutdown, framingResponses, framingRequests, connection) =>
        (commands, shutdown, framingResponses, framingRequests, connection)
    } { implicit builder =>
      (commands, shutdown, framingResponses, framingRequests, connection) =>

        import GraphDSL.Implicits._

        commands ~> connection.commands
                    connection.framingRequests  ~> framingRequests
                    connection.framingResponses <~ framingResponses

        shutdown      ~> connection.prepareForShutdown

        ClosedShape
    })

    val (commandsSource, shutdownSource, framingResponsesSource, framingRequestsSink, connectionDone) = graph.run()
  }

  it should "do something" in new mocks {
    commandsSource.ensureSubscription()
    shutdownSource.ensureSubscription()
    framingResponsesSource.ensureSubscription()
    framingRequestsSink.ensureSubscription()

    framingRequestsSink.request(1L)

    // emulate worker registration
    val queueMock = stub[SourceQueueWithComplete[RawResponse]]
    routingTable.put(0L, queueMock)

    commandsSource.sendNext(RenderedCommandWithToken(0L, ByteString("""[1,"abc",{}]""")))

    framingRequestsSink.expectNext() shouldBe ReqlFrame(0L, ByteString("""[1,"abc",{}]"""))

    val offerResultPromise = Promise[QueueOfferResult]()
    queueMock.offer _ when * returns offerResultPromise.future
    framingResponsesSource.sendNext(ReqlFrame(0L, ByteString(
      """{"t":3,"r":["abc","bcd"]}"""
    )))

    shutdownSource.sendNext(PrepareForShutdown)

    commandsSource.expectCancellation()
    shutdownSource.expectCancellation()
    framingResponsesSource.expectCancellation()
    framingRequestsSink.expectComplete()

    whenReady(connectionDone) { done =>
      done shouldBe Done
    }

    queueMock.offer _ verify RawResponse(ByteString(
      """{"t":3,"r":["abc","bcd"]}"""
    ))
  }

}

object LogicalConnectionStageTest {
  val config = """
    akka {
      loglevel = "WARNING"
    }
  """
}