package rere.driver.runners.ready

import akka.Done
import akka.stream.scaladsl.Sink

import scala.concurrent.Future

trait FiniteStreamReadyToGo[Out, Mat] {
  def drainTo(sink: Sink[Out, Mat]): (Future[Mat], Future[Done])
}
