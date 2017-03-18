package rere.driver.workers

import akka.actor.ActorRef
import akka.stream.ActorMaterializer
import rere.driver.logger.Logger

case class WorkerContext(
  materializer: ActorMaterializer,
  connectionRef: ActorRef,
  logger: Logger
)
