package rere.driver.connection

import akka.Done
import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.scaladsl.{Flow, Keep}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.testkit.TestKit
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.PatienceConfiguration.{Interval, Timeout}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.{FlatSpecLike, Matchers}
import rere.driver.connection.LogicalConnectionProtocol._

import scala.concurrent.duration._

class LogicalConnectionTest
  extends TestKit(ActorSystem("LogicalConnectionTest", ConfigFactory.parseString(LogicalConnectionTest.config)))
  with FlatSpecLike
  with ScalaFutures
  with Matchers
  with Eventually {

  trait mocks {
    implicit val mat = ActorMaterializer(
      ActorMaterializerSettings.create(system).withInputBuffer(16, 16)
    )
  }

  it should "do something" in new mocks {
    val connectionId = 0L
    val framingFlow = {
      val framingSource = TestSource.probe[ReqlFrame]
      val framingSink = TestSink.probe[ReqlFrame]
      Flow.fromSinkAndSourceMat(framingSink, framingSource)(Keep.both)
    }
    val connectionLogger = Logging(system, "connection-test")

    val ((framingSink, framingSource), logicalConnection) =
      LogicalConnection.connect(connectionId, framingFlow, connectionLogger)

    framingSink.ensureSubscription()
    framingSource.ensureSubscription()

    val workerFlow = {
      val commandsSource = TestSource.probe[RenderedCommand]
      val responseSink = TestSink.probe[RawResponse]
      Flow.fromSinkAndSourceMat(responseSink, commandsSource)(Keep.both)
    }

    val connectionFlow = logicalConnection.createConnectionFlow()

    val (responseSink, commandsSource) = workerFlow.join(connectionFlow).run()

    commandsSource.sendNext(RenderedCommand(ByteString("""[1,"abc",{}]""")))
    responseSink.request(1L)

    framingSink.request(1L)
    framingSink.expectNext() shouldBe ReqlFrame(0L, ByteString("""[1,"abc",{}]"""))

    framingSource.sendNext(ReqlFrame(0L, ByteString(
      """{"t":3,"r":["abc","bcd"]}"""
    )))

    responseSink.expectNext() shouldBe RawResponse(ByteString(
      """{"t":3,"r":["abc","bcd"]}"""
    ))

    logicalConnection.isBusyToken(0L) shouldBe true

    whenReady(logicalConnection.prepareForShutdown()) { ready =>
      ready shouldBe an [Done]
    }

    framingSource.expectCancellation()
    framingSink.expectComplete()

    commandsSource.expectCancellation()
    responseSink.expectComplete()

    whenReady(logicalConnection.done) { done =>
      done shouldBe Done
    }

    eventually(Timeout(3.seconds), Interval(100.millis)) {
      logicalConnection.isBusyToken(0L) shouldBe false
    }
  }

}

object LogicalConnectionTest {
  val config = """
    akka {
      loglevel = "WARNING"
    }
  """
}