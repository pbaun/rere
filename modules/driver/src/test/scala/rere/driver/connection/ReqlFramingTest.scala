package rere.driver.connection

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.testkit.TestKit
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import org.scalatest.{AsyncFlatSpecLike, BeforeAndAfterAll, Matchers}

import scala.concurrent.Future

class ReqlFramingTest
  extends TestKit(ActorSystem("ReqlFramingTest", ConfigFactory.parseString(ReqlFramingTest.config)))
  with AsyncFlatSpecLike
  with BeforeAndAfterAll
  with Matchers {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
    super.afterAll()
  }

  behavior of "ReqlFraming.renderFlow"

  it should "render regular frames" in {
    val frames = ReqlFrame(1L, ByteString(0x01, 0x02, 0x03, 0x04, 0x05)) :: Nil
    val expected =
      ByteString(0x01, 0x00, 0x00, 0x00) ++
      ByteString(0x00, 0x00, 0x00, 0x00) ++
      ByteString(0x05, 0x00, 0x00, 0x00) ++
      ByteString(0x01, 0x02, 0x03, 0x04, 0x05)

    implicit val mat = ActorMaterializer()
    val source = Source(frames)
    val sink = Sink.fold[ByteString, ByteString](ByteString.empty)((acc, frame) => acc ++ frame)

    val graph = source.via(ReqlFraming.renderFlow).toMat(sink)(Keep.right)
    val probe = graph.run()

    probe.map {
      _ shouldBe expected
    }
  }

  it should "render frame with zero size body" in {
    val frames = ReqlFrame(1L, ByteString.empty) :: Nil
    val expected =
      ByteString(0x01, 0x00, 0x00, 0x00) ++
      ByteString(0x00, 0x00, 0x00, 0x00) ++
      ByteString(0x00, 0x00, 0x00, 0x00)

    implicit val mat = ActorMaterializer()
    val source = Source(frames)
    val sink = Sink.fold[ByteString, ByteString](ByteString.empty)((acc, frame) => acc ++ frame)

    val graph = source.via(ReqlFraming.renderFlow).toMat(sink)(Keep.right)
    val probe = graph.run()

    probe.map {
      _ shouldBe expected
    }
  }


  behavior of "ReqlFraming.parsingFlow"

  it should "concat incoming bytes and parse they to frame" in {
    val chunks =
      ByteString(0x01, 0x00, 0x00, 0x00) ::
      ByteString(0x00, 0x00, 0x00, 0x00) ::
      ByteString(0x05, 0x00, 0x00, 0x00) ::
      ByteString(0x01, 0x02) ::
      ByteString(0x03) ::
      ByteString(0x04, 0x05) ::
      Nil

    val expected = ReqlFrame(1L, ByteString(0x01, 0x02, 0x03, 0x04, 0x05)) :: Nil

    implicit val mat = ActorMaterializer()
    val source = Source(chunks)
    val sink = Sink.fold[Seq[ReqlFrame], ReqlFrame](Seq.empty)((acc, frame) => acc :+ frame)

    val graph = source.via(ReqlFraming.parsingFlow).toMat(sink)(Keep.right)
    val probe = graph.run()

    probe.map {
      _ shouldBe expected
    }
  }

  it should "concat incoming bytes and parse they to many frames" in {
    val chunks =
      // 1
      ByteString(0x01, 0x00, 0x00, 0x00) ::
      ByteString(0x00, 0x00, 0x00, 0x00) ::
      ByteString(0x05, 0x00, 0x00, 0x00) ::
      ByteString(0x01, 0x02) ::
      ByteString(0x03) ::
      ByteString(0x04, 0x05, 0x02, 0x00) ::
      // 2
      ByteString(0x00, 0x00, 0x00, 0x00, 0x00) ::
      ByteString(0x00, 0x03, 0x00, 0x00, 0x00, 0x06, 0x07) ::
      ByteString(0x08) ::
      Nil

    val expected =
      ReqlFrame(1L, ByteString(0x01, 0x02, 0x03, 0x04, 0x05)) ::
      ReqlFrame(2L, ByteString(0x06, 0x07, 0x08)) ::
      Nil

    implicit val mat = ActorMaterializer()
    val source = Source(chunks)
    val sink = Sink.fold[Seq[ReqlFrame], ReqlFrame](Seq.empty)((acc, frame) => acc :+ frame)

    val graph = source.via(ReqlFraming.parsingFlow).toMat(sink)(Keep.right)
    val probe = graph.run()

    probe.map {
      _ shouldBe expected
    }
  }

  it should "fail when upstream finishing at the middle of first frame" in {
    val chunks =
      ByteString(0x01, 0x00, 0x00, 0x00) ::
      ByteString(0x00, 0x00, 0x00, 0x00) ::
      ByteString(0x05, 0x00, 0x00, 0x00) ::
      ByteString(0x01, 0x02) ::
      ByteString(0x03) ::
      Nil

    implicit val mat = ActorMaterializer()
    val source = Source(chunks)
    val sink = Sink.fold[Seq[ReqlFrame], ReqlFrame](Seq.empty)((acc, frame) => acc :+ frame)

    val graph = source.via(ReqlFraming.parsingFlow).toMat(sink)(Keep.right)
    val probe = graph.run()

    recoverToSucceededIf[ReqlFramingException](probe)

    probe.failed.map {
      _ shouldBe ReqlFramingException(
        "Incoming data stream was finished at the middle of frame"
      )
    }
  }

  it should "fail when upstream finishing at the middle of second frame" in {
    val chunks =
      ByteString(0x01, 0x00, 0x00, 0x00) ::
      ByteString(0x00, 0x00, 0x00, 0x00) ::
      ByteString(0x05, 0x00, 0x00, 0x00) ::
      ByteString(0x01, 0x02) ::
      ByteString(0x03) ::
      ByteString(0x04, 0x05, 0x00) ::               // 1 extra byte
      Nil

    implicit val mat = ActorMaterializer()
    val source = Source(chunks)
    val sink = Sink.fold[Seq[ReqlFrame], ReqlFrame](Seq.empty)((acc, frame) => acc :+ frame)

    val graph = source.via(ReqlFraming.parsingFlow).toMat(sink)(Keep.right)

    val probe = graph.run()
    recoverToSucceededIf[ReqlFramingException](probe)

    probe.failed.map {
      _ shouldBe ReqlFramingException(
        "Incoming data stream was finished at the middle of frame"
      )
    }
  }

  it should "not fail when downstream finishing before all bytes was parsed" in {
    val chunks =
      // 1
      ByteString(0x01, 0x00, 0x00, 0x00) ::
      ByteString(0x00, 0x00, 0x00, 0x00) ::
      ByteString(0x05, 0x00, 0x00, 0x00) ::
      ByteString(0x01, 0x02) ::
      ByteString(0x03) ::
      ByteString(0x04, 0x05, 0x02, 0x00) ::
      // 2
      ByteString(0x00, 0x00, 0x00, 0x00, 0x00) ::
      ByteString(0x00, 0x03, 0x00, 0x00, 0x00, 0x06, 0x07) :: // not completed frame
      Nil

    val expected =
      ReqlFrame(1L, ByteString(0x01, 0x02, 0x03, 0x04, 0x05)) ::
      Nil

    implicit val mat = ActorMaterializer()
    val source = Source.actorRef[ByteString](32, OverflowStrategy.fail)

    // it will take only first frame and cancel upstream
    val limit = Flow[ReqlFrame].take(1)
    val sink = Sink.fold[Seq[ReqlFrame], ReqlFrame](Seq.empty)((acc, frame) => acc :+ frame)
    val limitedSink: Sink[ReqlFrame, Future[Seq[ReqlFrame]]] = limit.toMat(sink)(Keep.right)

    val graph = source.via(ReqlFraming.parsingFlow).toMat(limitedSink)(Keep.both)

    val (ref, probe) = graph.run()

    // sending many bytes but not completing source
    chunks.foreach { chunk =>
      ref ! chunk
    }

    probe.map {
      _ shouldBe expected
    }
  }


  behavior of "ReqlFraming.framingBidi"

  it should "be able to eat own dog food" in {
    val frames = ReqlFrame(1L, ByteString(0x01, 0x02, 0x03, 0x04, 0x05)) :: Nil

    implicit val mat = ActorMaterializer()
    val source = Source(frames)
    val sink = Sink.fold[Seq[ReqlFrame], ReqlFrame](Seq.empty)((acc, frame) => acc :+ frame)
    val tcpLoop = Flow[ByteString].map(identity)

    val graph = source.via(ReqlFraming.framingBidi.join(tcpLoop)).toMat(sink)(Keep.right)
    val probe = graph.run()

    probe.map {
      _ shouldBe frames
    }
  }

}

object ReqlFramingTest {
  val config = """
    akka {
      loglevel = "WARNING"
    }
  """
}
