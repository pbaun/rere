package rere.driver.connection

import java.nio.ByteOrder

import akka.NotUsed
import akka.stream._
import akka.stream.scaladsl.{BidiFlow, Flow}
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.util.{ByteString, ByteStringBuilder}

import scala.annotation.tailrec

object ReqlFraming {

  val renderFlow: Flow[ReqlFrame, ByteString, NotUsed] =
    Flow.fromGraph(new ReqlOutFraming())

  val parsingFlow: Flow[ByteString, ReqlFrame, NotUsed] =
    Flow.fromGraph(new ReqlInFraming()).mapConcat(identity)

  val framingBidi: BidiFlow[ReqlFrame, ByteString, ByteString, ReqlFrame, NotUsed] =
    BidiFlow.fromFlows(renderFlow, parsingFlow)

}

final case class ReqlFrame(queryToken: Long, body: ByteString)

final case class ReqlFramingException(msg: String) extends Exception(msg)

final class ReqlOutFraming extends GraphStage[FlowShape[ReqlFrame, ByteString]] {
  val incomingFrames = Inlet[ReqlFrame]("OutFraming.incomingFrames")
  val outgoingData = Outlet[ByteString]("OutFraming.outgoingData")

  override val shape = FlowShape.of(incomingFrames, outgoingData)

  override def createLogic(att: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) {

      private val BYTE_ORDER: ByteOrder = ByteOrder.LITTLE_ENDIAN

      setHandler(incomingFrames, new InHandler {
        override def onPush(): Unit = {
          val frame = grab(incomingFrames)
          val data = new ByteStringBuilder()
            .putLong(frame.queryToken)(BYTE_ORDER)
            .putInt(frame.body.size)(BYTE_ORDER)
            .++=(frame.body)
            .result()
          push(outgoingData, data)
        }
      })

      setHandler(outgoingData, new OutHandler {
        override def onPull(): Unit = {
          pull(incomingFrames)
        }
      })
    }
}

final class ReqlInFraming extends GraphStage[FlowShape[ByteString, List[ReqlFrame]]] {
  val incomingData = Inlet[ByteString]("InFraming.incomingData")
  val outgoingFrames = Outlet[List[ReqlFrame]]("InFraming.outgoingFrames")

  override val shape = FlowShape.of(incomingData, outgoingFrames)

  override def createLogic(att: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) {

      private val BYTE_ORDER: ByteOrder = ByteOrder.LITTLE_ENDIAN
      private val HEADER_SIZE: Int = 12

      private var queryToken: Long = 0L
      private var bodySize: Int = Int.MinValue
      private var buffer: ByteString = ByteString.empty
      private var isHeaderParsed: Boolean = false

      private def parseHeader(): Unit = {
        val it = buffer.iterator
        queryToken = it.getLong(BYTE_ORDER)
        bodySize = it.getInt(BYTE_ORDER)
        isHeaderParsed = true
        buffer = buffer.drop(HEADER_SIZE)
      }

      private def forgiveHeader(): Unit = {
        queryToken = 0L
        bodySize = Int.MinValue
        isHeaderParsed = false
      }

      private def makeFrame(): ReqlFrame = {
        val frame = ReqlFrame(queryToken, buffer.take(bodySize))
        buffer = buffer.drop(bodySize)
        forgiveHeader()
        frame
      }

      @tailrec
      private def parseTR(acc: List[ReqlFrame]): List[ReqlFrame] = {
        if (!isHeaderParsed && buffer.size >= HEADER_SIZE) {
          parseHeader()
        }
        if (!isHeaderParsed || buffer.size < bodySize) {
          acc
        } else {
          parseTR(makeFrame() :: acc)
        }
      }

      setHandler(incomingData, new InHandler {
        override def onPush(): Unit = {
          buffer = buffer ++ grab(incomingData)
          push(outgoingFrames, parseTR(Nil).reverse)
        }

        override def onUpstreamFinish(): Unit = {
          if (buffer.nonEmpty) {
            failStage(ReqlFramingException("Incoming data stream was finished at the middle of frame"))
          } else {
            completeStage()
          }
        }
      })

      setHandler(outgoingFrames, new OutHandler {
        override def onPull(): Unit = {
          pull(incomingData)
        }

        override def onDownstreamFinish(): Unit = {
          // It is ok that buffer contains some amount of not parsed data; downstream do not need them anyway
          completeStage()
        }
      })
    }
}
