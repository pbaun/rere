package rere.driver.runners.ready

import akka.stream.scaladsl.Sink

trait InfiniteStreamReadyToGo[Out, OutMat] {
  def drainTo(sink: Sink[Out, OutMat]): OutMat
}
