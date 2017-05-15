package rere.driver.connection

import akka.event.LoggingAdapter
import akka.stream._
import akka.stream.scaladsl.{Flow, GraphDSL}
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.util.ByteString

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

class Switcher(
    logger: LoggingAdapter
  ) extends GraphStage[BidiMergeShape[ByteString, ByteString, ByteString, ByteString, ByteString, ByteString]] {

  val authToServer = Inlet[ByteString]("Switcher.authToServer")
  val dataToServer = Inlet[ByteString]("Switcher.dataToServer")
  val toServer = Outlet[ByteString]("Switcher.toServer")

  val fromServer = Inlet[ByteString]("Switcher.fromServer")
  val authFromServer = Outlet[ByteString]("Switcher.authFromServer")
  val dataFromServer = Outlet[ByteString]("Switcher.dataFromServer")

  override val shape = new BidiMergeShape(authToServer, dataToServer, toServer, fromServer, authFromServer, dataFromServer)

  override def createLogic(att: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) {

      var authCompleted = false

      setHandler(authToServer, new InHandler {
        override def onPush(): Unit = {
          logger.debug("onPush authToServer")
          if (!authCompleted) {
            push(toServer, grab(authToServer))
          } else {
            failStage(new IllegalStateException("push from auth inlet after auth completed"))
          }
        }

        override def onUpstreamFinish(): Unit = {
          logger.debug("onFinish authToServer")
          complete(authFromServer)

          if (isAvailable(toServer)) {
            logger.debug("+dataToServer+")
            pull(dataToServer)
          } else {
            logger.debug("-dataToServer-")
          }

          if (isAvailable(dataFromServer)) {
            logger.debug("+fromServer+")
            pull(fromServer)
          } else {
            logger.debug("-fromServer-")
          }

          authCompleted = true
        }
      })

      setHandler(dataToServer, new InHandler {
        override def onPush(): Unit = {
          logger.debug("onPush dataToServer")
          if (authCompleted) {
            push(toServer, grab(dataToServer))
          } else {
            failStage(new IllegalStateException("push from data inlet before auth completed"))
          }
        }

        override def onUpstreamFinish(): Unit = {
          logger.debug("onFinish dataToServer")
          complete(toServer)
        }
      })

      setHandler(toServer, new OutHandler {
        override def onPull(): Unit = {
          logger.debug("onPull toServer")
          if (!authCompleted) {
            pull(authToServer)
          } else {
            pull(dataToServer)
          }
        }
      })

      setHandler(fromServer, new InHandler {
        override def onPush(): Unit = {
          logger.debug("onPush fromServer")
          if (!authCompleted) {
            push(authFromServer, grab(fromServer))
          } else {
            push(dataFromServer, grab(fromServer))
          }
        }
      })

      setHandler(authFromServer, new OutHandler {
        override def onPull(): Unit = {
          logger.debug("onPull authFromServer")
          if (!authCompleted) {
            pull(fromServer)
          }
        }
      })

      setHandler(dataFromServer, new OutHandler {
        override def onPull(): Unit = {
          logger.debug("onPull dataFromServer")
          if (authCompleted) {
            pull(fromServer)
          }
        }
      })
    }

}

object Switcher {
  def toDataFlow[M1, M2](
    connection: Flow[ByteString, ByteString, M1],
    commander: Flow[ByteString, ByteString, M2],
    switcherLogger: LoggingAdapter
  ): Flow[ByteString, ByteString, M1] = {
    Flow.fromGraph(GraphDSL.create(
      connection, commander, new Switcher(switcherLogger)) {
      (connectionM, commanderM, switcherM) => connectionM
    } { implicit builder =>
      (connection, commander, switcher) =>

        import GraphDSL.Implicits._

        commander.out ~> switcher.in1
        switcher.out1 ~> connection.in
        switcher.in3  <~ connection.out
        commander.in  <~ switcher.out2

        FlowShape.of(switcher.in2, switcher.out3)
    })
  }
}
