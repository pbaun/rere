package rere.driver.util

import akka.NotUsed
import akka.event.LoggingAdapter
import akka.stream.scaladsl.{BidiFlow, Flow}
import akka.util.ByteString

object StreamsDebugging {

  def loggingBidi(logger: LoggingAdapter): BidiFlow[ByteString, ByteString, ByteString, ByteString, NotUsed] =
    BidiFlow.fromFlows(
      loggingFlow("from server", logger),
      loggingFlow("to server", logger)
    )

  private def loggingFlow[M](tag: String, logger: LoggingAdapter): Flow[ByteString, ByteString, NotUsed] =
    Flow[ByteString].map { buf =>
      if (logger.isDebugEnabled) {
        logger.debug("{}: {} {}", tag, buf.utf8String, toHex(buf))
      }
      buf
    }

  private def toHex(buf: ByteString): String = {
    buf.map(x => "%02X" format x).mkString
  }

}
