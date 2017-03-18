package rere.driver.pool

import akka.actor.{ActorRef, Props}
import rere.driver.exceptions.ReqlDriverError
import rere.driver.protocol.ReqlExpectedResponseType
import rere.driver.workers.WorkerContext

import scala.concurrent.Promise

sealed trait ConnectionPoolMessages

sealed trait ConnectionPoolIncomingMessages extends ConnectionPoolMessages

final case class AcquireConnection(
  expectedResponseType: ReqlExpectedResponseType,
  workerProps: WorkerContext => Props,
  onConnectionFail: (ReqlDriverError) => Unit
) extends ConnectionPoolIncomingMessages

final case class Shutdown(whenShutdown: Promise[ConnectionPoolShutdownResult]) extends ConnectionPoolIncomingMessages

final case class ConnectionProblem(connectionRef: ActorRef, cause: ReqlDriverError) extends ConnectionPoolIncomingMessages