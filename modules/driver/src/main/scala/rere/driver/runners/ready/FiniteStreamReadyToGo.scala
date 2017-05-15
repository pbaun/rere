package rere.driver.runners.ready

import akka.stream.scaladsl.Sink

trait FiniteStreamReadyToGo[Out, OutMat] {
  def drainTo(sink: Sink[Out, OutMat]): OutMat
}
