package rere.driver.connection

import akka.stream.ClosedShape
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph}
import akka.util.ByteString

object Interconnections {
  def connectToSwitcher[M1, M2, M3, MOut](
    connection: Flow[ByteString, ByteString, M1],
    commander: Flow[ByteString, ByteString, M2],
    dataFlow: Flow[ByteString, ByteString, M3]
  ): RunnableGraph[(M1, M3)] = {

    RunnableGraph.fromGraph(GraphDSL.create(
      connection, commander, dataFlow, new Switcher()) {
      (connectionM, commanderM, dataFlowM, switcherM) => (connectionM, dataFlowM)
    } { implicit builder =>
      (connection, commander, dataFlow, switcher) =>

        import GraphDSL.Implicits._

        commander.out ~> switcher.in1
        dataFlow.out  ~> switcher.in2
        switcher.out1 ~> connection.in
        switcher.in3  <~ connection.out
        commander.in  <~ switcher.out2
        dataFlow.in   <~ switcher.out3

        ClosedShape
    })
  }
}
