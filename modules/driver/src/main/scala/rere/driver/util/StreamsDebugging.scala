package rere.driver.util

import akka.stream.scaladsl.{BidiFlow, Flow, Keep}
import akka.util.ByteString

object StreamsDebugging {

  val verbose = true

  def log(x: Any): Unit = if (verbose) println(x)

  def logging(tag: String) = {
    Flow[ByteString].map { x =>
      val buf = x.toArray
      val str = buf.map("%02X" format _).mkString
      log(s"$tag: ${x.utf8String} ${str}")
      x
    }
  }
  val loggingBidi = BidiFlow.fromFlowsMat(logging("from server"), logging("to server"))(Keep.left)

  def toHex(buf: ByteString): String = {
    buf.map(x => "%02X" format x).mkString
  }
}
