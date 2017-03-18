package rere.driver.connection

import akka.stream._
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.util.ByteString
import rere.driver.util.StreamsDebugging

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable

class BidiMergeShape[-In1, -In2, +Out1, -In3, +Out2, +Out3](
    val in1: Inlet[In1 @uncheckedVariance],
    val in2: Inlet[In2 @uncheckedVariance],
    val out1: Outlet[Out1 @uncheckedVariance],
    val in3: Inlet[In3 @uncheckedVariance],
    val out2: Outlet[Out2 @uncheckedVariance],
    val out3: Outlet[Out3 @uncheckedVariance]
  ) extends Shape {

  override val inlets: immutable.Seq[Inlet[_]] = List(in1, in2, in3)
  override val outlets: immutable.Seq[Outlet[_]] = List(out1, out2, out3)

  override def deepCopy(): BidiMergeShape[In1, In2, Out1, In3, Out2, Out3] = {
    new BidiMergeShape(
      in1.carbonCopy(), in2.carbonCopy(), out1.carbonCopy(),
      in3.carbonCopy(), out2.carbonCopy(), out3.carbonCopy()
    )
  }
  override def copyFromPorts(inlets: immutable.Seq[Inlet[_]], outlets: immutable.Seq[Outlet[_]]): Shape = {
    require(inlets.size == 3, s"proposed inlets [${inlets.mkString(", ")}] do not fit BidiMergeShape")
    require(outlets.size == 3, s"proposed outlets [${outlets.mkString(", ")}] do not fit BidiMergeShape")
    new BidiMergeShape(inlets(0), inlets(1), outlets(0), inlets(2), outlets(1), outlets(2))
  }
  def reversed: Shape = copyFromPorts(inlets.reverse, outlets.reverse)
}

class Switcher extends GraphStage[BidiMergeShape[ByteString, ByteString, ByteString, ByteString, ByteString, ByteString]] {

  val authToServer = Inlet[ByteString]("Switcher.authToServer")
  val dataToServer = Inlet[ByteString]("Switcher.dataToServer")
  val toServer = Outlet[ByteString]("Switcher.toServer")

  val fromServer = Inlet[ByteString]("Switcher.fromServer")
  val authFromServer = Outlet[ByteString]("Switcher.authFromServer")
  val dataFromServer = Outlet[ByteString]("Switcher.dataFromServer")

  override val shape = new BidiMergeShape(authToServer, dataToServer, toServer, fromServer, authFromServer, dataFromServer)

  override def createLogic(att: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) {

      def log(x: Any) = StreamsDebugging.log(x)

      var authCompleted = false

      setHandler(authToServer, new InHandler {
        override def onPush(): Unit = {
          log("onPush authToServer")
          if (!authCompleted) {
            push(toServer, grab(authToServer))
          } else {
            failStage(new IllegalStateException("push from auth inlet after auth completed"))
          }
        }

        override def onUpstreamFinish(): Unit = {
          log("onFinish authToServer")
          complete(authFromServer)

          if (isAvailable(toServer)) {
            log("+dataToServer+")
            pull(dataToServer)
          } else {
            log("-dataToServer-")
          }

          if (isAvailable(dataFromServer)) {
            log("+fromServer+")
            pull(fromServer)
          } else {
            log("-fromServer-")
          }

          authCompleted = true
        }
      })

      setHandler(dataToServer, new InHandler {
        override def onPush(): Unit = {
          log("onPush dataToServer")
          if (authCompleted) {
            push(toServer, grab(dataToServer))
          } else {
            failStage(new IllegalStateException("push from data inlet before auth completed"))
          }
        }

        override def onUpstreamFinish(): Unit = {
          log("onFinish dataToServer")
          complete(toServer)
        }
      })

      setHandler(toServer, new OutHandler {
        override def onPull(): Unit = {
          log("onPull toServer")
          if (!authCompleted) {
            pull(authToServer)
          } else {
            pull(dataToServer)
          }
        }
      })

      setHandler(fromServer, new InHandler {
        override def onPush(): Unit = {
          log("onPush fromServer")
          if (!authCompleted) {
            push(authFromServer, grab(fromServer))
          } else {
            push(dataFromServer, grab(fromServer))
          }
        }
      })

      setHandler(authFromServer, new OutHandler {
        override def onPull(): Unit = {
          log("onPull authFromServer")
          if (!authCompleted) {
            pull(fromServer)
          }
        }
      })

      setHandler(dataFromServer, new OutHandler {
        override def onPull(): Unit = {
          log("onPull dataFromServer")
          if (authCompleted) {
            pull(fromServer)
          }
        }
      })
    }

}

