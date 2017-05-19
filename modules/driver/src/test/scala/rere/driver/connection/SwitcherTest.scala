package rere.driver.connection

import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream._
import akka.stream.scaladsl.{GraphDSL, RunnableGraph}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.testkit.TestKit
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpecLike, Matchers}

import scala.concurrent.duration._

class SwitcherTest
  extends TestKit(ActorSystem("SwitcherTest", ConfigFactory.parseString(SwitcherTest.config)))
  with FlatSpecLike
  with Matchers {

  trait mocks {

    val logger = Logging(system, "switcher-test")

    def logStart(tag: String): Unit = logger.debug("start {}", tag)
    def logEnd(tag: String): Unit = logger.debug("end {}", tag)

    implicit val mat = ActorMaterializer(
      ActorMaterializerSettings.create(system).withInputBuffer(16, 16)
    )

    def connect() = {
      RunnableGraph.fromGraph(GraphDSL.create(
        TestSource.probe[ByteString],
        TestSource.probe[ByteString],
        TestSink.probe[ByteString],
        TestSource.probe[ByteString],
        TestSink.probe[ByteString],
        TestSink.probe[ByteString],
        new Switcher(logger)
      ) (
        (authSource, dataSource, toServerSink, fromServerSource, authSink, dataSink, switcher) =>
          (authSource, dataSource, toServerSink, fromServerSource, authSink, dataSink, switcher)
      ) { implicit b =>
        (authSource, dataSource, toServerSink, fromServerSource, authSink, dataSink, switcher) =>

          import GraphDSL.Implicits._

          authSource.out ~> switcher.upstreamAIn
          dataSource.out ~> switcher.upstreamBIn
                            switcher.upstreamOut    ~> toServerSink.in
                            switcher.downstreamIn   <~ fromServerSource.out
          authSink.in    <~ switcher.downstreamAOut
          dataSink.in    <~ switcher.downstreamBOut

          ClosedShape
      })
    }

    val (authSource, dataSource, toServerSink, fromServerSource, authSink, dataSink, switcher) = connect().run()
  }

  behavior of "Switcher"

  it should "switch after auth complete and start to transfer data" in new mocks {
    logStart("#1")

    authSource.ensureSubscription()
    toServerSink.ensureSubscription()
    dataSource.ensureSubscription()

    dataSource.sendNext(ByteString("data data data"))       //<- data ready before auth
    dataSink.request(1L)                                    //<- response demand before auth complete

    authSource.sendNext(ByteString("fast auth"))
    authSink.request(1L)

    toServerSink.request(1L)
    toServerSink.expectNext(ByteString("fast auth"))
    fromServerSource.sendNext(ByteString("auth ok"))
    toServerSink.request(1L)                                //<- packets demand before auth complete

    authSink.expectNext(ByteString("auth ok"))
    authSource.sendComplete()
    authSink.expectComplete()

    toServerSink.expectNext(ByteString("data data data"))   //<- start of data transmission
    fromServerSource.sendNext(ByteString("saved"))
    toServerSink.request(1L)

    dataSink.expectNext(ByteString("saved"))
    dataSource.sendNext(ByteString("data data data2"))
    dataSink.request(1L)
    dataSource.sendComplete()

    toServerSink.expectNext(ByteString("data data data2"))
    toServerSink.expectComplete()

    fromServerSource.sendNext(ByteString("saved2"))
    fromServerSource.sendComplete()

    dataSink.expectNext(ByteString("saved2"))
    dataSink.expectComplete()

    logEnd("#1")
  }

  it should "correctly switch if data response is demands after auth" in new mocks {
    logStart("#2")
    authSource.ensureSubscription()
    toServerSink.ensureSubscription()
    dataSource.ensureSubscription()

    dataSource.sendNext(ByteString("data data data"))       //<- try to send before demand response and auth complete

    authSource.sendNext(ByteString("fast auth"))
    authSink.request(1L)

    toServerSink.request(1L)
    toServerSink.expectNext(ByteString("fast auth"))
    fromServerSource.sendNext(ByteString("auth ok"))
    toServerSink.request(1L)                                //<- demand packets before auth complete

    authSink.expectNext(ByteString("auth ok"))
    authSource.sendComplete()
    authSink.expectComplete()

    toServerSink.expectNext(ByteString("data data data"))   //<- start of data transmission
    fromServerSource.sendNext(ByteString("saved"))
    toServerSink.request(1L)

    dataSink.request(1L)                                    //<- demand response after auth complete
    dataSink.expectNext(ByteString("saved"))
    dataSource.sendNext(ByteString("data data data2"))
    dataSink.request(1L)
    dataSource.sendComplete()

    toServerSink.expectNext(ByteString("data data data2"))
    toServerSink.expectComplete()

    fromServerSource.sendNext(ByteString("saved2"))
    fromServerSource.sendComplete()

    dataSink.expectNext(ByteString("saved2"))
    dataSink.expectComplete()

    logEnd("#2")
  }

  it should "switch correctly if data is ready only after auth complete" in new mocks {
    logStart("#3")
    authSource.ensureSubscription()
    toServerSink.ensureSubscription()
    dataSource.ensureSubscription()

    authSink.request(1L)
    toServerSink.request(1L)

    authSource.sendNext(ByteString("fast auth"))
    toServerSink.expectNext(ByteString("fast auth"))
    fromServerSource.sendNext(ByteString("auth ok"))
    toServerSink.request(1L)                                          //<- demand packets before auth complete

    authSink.expectNext(ByteString("auth ok"))
    authSource.sendComplete()
    authSink.expectComplete()

    dataSource.sendNext(ByteString("data data data"))                 //<- start send data after auth
    dataSink.request(1L)                                              //<- demand response after auth complete

    toServerSink.expectNext(ByteString("data data data"))             //<- start of data transmission
    fromServerSource.sendNext(ByteString("saved"))
    toServerSink.request(1L)

    dataSink.expectNext(ByteString("saved"))
    dataSink.request(1L)

    dataSource.sendNext(ByteString("data data data2"))
    dataSource.sendComplete()
    toServerSink.expectNext(ByteString("data data data2"))
    toServerSink.expectComplete()

    fromServerSource.sendNext(ByteString("saved2"))
    fromServerSource.sendComplete()
    dataSink.expectNext(ByteString("saved2"))
    dataSink.expectComplete()

    logEnd("#3")
  }

  it should "switch correctly if data ready only after auth and packets demands only after auth" in new mocks {
    logStart("#4")
    authSource.ensureSubscription()
    toServerSink.ensureSubscription()
    dataSource.ensureSubscription()

    authSink.request(1L)
    toServerSink.request(1L)

    authSource.sendNext(ByteString("fast auth"))
    toServerSink.expectNext(ByteString("fast auth"))
    fromServerSource.sendNext(ByteString("auth ok"))

    authSink.expectNext(ByteString("auth ok"))
    authSource.sendComplete()
    authSink.expectComplete()

    dataSource.sendNext(ByteString("data data data"))           //<- send packets after auth complete
    dataSink.request(1L)                                        //<- demand response after auth complete

    toServerSink.request(1L)                                    //<- demand packets after auth complete
    toServerSink.expectNext(ByteString("data data data"))       //<- start of data transmission
    fromServerSource.sendNext(ByteString("saved"))
    toServerSink.request(1L)

    dataSink.expectNext(ByteString("saved"))
    dataSink.request(1L)

    dataSource.sendNext(ByteString("data data data2"))
    dataSource.sendComplete()
    toServerSink.expectNext(ByteString("data data data2"))
    toServerSink.expectComplete()

    fromServerSource.sendNext(ByteString("saved2"))
    fromServerSource.sendComplete()
    dataSink.expectNext(ByteString("saved2"))
    dataSink.expectComplete()

    logEnd("#4")
  }

  it should "switch correctly if data ready before auth and packets demands only after auth" in new mocks {
    logStart("#5")
    authSource.ensureSubscription()
    toServerSink.ensureSubscription()
    dataSource.ensureSubscription()

    authSink.request(1L)
    toServerSink.request(1L)

    authSource.sendNext(ByteString("fast auth"))
    toServerSink.expectNext(ByteString("fast auth"))
    fromServerSource.sendNext(ByteString("auth ok"))

    dataSink.request(1L)                                        //<- demand data response before auth
    dataSource.sendNext(ByteString("data data data"))           //<- send data before auth complete

    authSink.expectNext(ByteString("auth ok"))
    authSource.sendComplete()
    authSink.expectComplete()

    toServerSink.request(1L)                                    //<- demand packets after auth complete

    toServerSink.expectNext(ByteString("data data data"))       //<- start of data transmission
    fromServerSource.sendNext(ByteString("saved"))
    toServerSink.request(1L)

    dataSink.expectNext(ByteString("saved"))
    dataSink.request(1L)

    dataSource.sendNext(ByteString("data data data2"))
    dataSource.sendComplete()
    toServerSink.expectNext(ByteString("data data data2"))
    toServerSink.expectComplete()

    fromServerSource.sendNext(ByteString("saved2"))
    fromServerSource.sendComplete()
    dataSink.expectNext(ByteString("saved2"))
    dataSink.expectComplete()

    logEnd("#5")
  }

  it should "not push to data output if it is not ready to consume data" in new mocks {
    logStart("#6")
    authSource.ensureSubscription()
    toServerSink.ensureSubscription()
    dataSource.ensureSubscription()

    authSink.request(1L)
    toServerSink.request(1L)

    authSource.sendNext(ByteString("fast auth"))
    toServerSink.expectNext(ByteString("fast auth"))
    fromServerSource.sendNext(ByteString("auth ok"))
    toServerSink.request(1L)                                          //<- demand packets before auth complete

    authSink.expectNext(ByteString("auth ok"))
    authSource.sendComplete()
    authSink.expectComplete()

    dataSource.sendNext(ByteString("data data data"))                 //<- start send data after auth

    toServerSink.expectNext(ByteString("data data data"))             //<- start of data transmission
    fromServerSource.unsafeSendNext(ByteString("saved"))
    fromServerSource.unsafeSendNext(ByteString("saved2"))
    toServerSink.request(1L)

    dataSink.ensureSubscription()
    dataSink.expectNoMsg(1.second)

    dataSink.request(1L)                                              //<- demand response after transmission start
    dataSink.expectNext(ByteString("saved"))
    dataSink.expectNoMsg(1.second)
    dataSink.request(1L)
    dataSink.expectNext(ByteString("saved2"))

    dataSource.sendComplete()
    toServerSink.expectComplete()

    fromServerSource.sendComplete()
    dataSink.expectComplete()

    logEnd("#6")
  }

}

object SwitcherTest {

  val config = """
    akka {
      loglevel = "DEBUG"
    }
  """
}
