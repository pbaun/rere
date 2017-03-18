package rere.driver.connection

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.stream.actor._
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.util.ByteString
import rere.driver.exceptions.ReqlDriverError
import rere.driver.logger.{LOGICAL_CONNECTION, Logger}
import rere.driver.pool.ConnectionProblem

import scala.collection.mutable

class LogicalConnection(logger: Logger) extends Actor with ActorSubscriber with ActorPublisher[ReqlFrame] {

  import LogicalConnectionProtocol._

  case class BufferedRenderedCommand(
    command: LogicalConnectionProtocol.RenderedCommand,
    sender: ActorRef)

  //TODO: it's good or bad?
  val requestStrategy = WatermarkRequestStrategy(16)

  var tokenCounter: Long = 0L
  val tokenToWorker: mutable.Map[Long, ActorRef] = mutable.Map.empty
  val workerToToken: mutable.Map[ActorRef, Long] = mutable.Map.empty
  val buffer: mutable.ListBuffer[BufferedRenderedCommand] = mutable.ListBuffer.empty

  private def sendNext(command: RenderedCommand, commandSender: ActorRef): Unit = {
    logger.log(LOGICAL_CONNECTION, self.toString(), s"sendNext: $commandSender")
    workerToToken.get(commandSender) match {
      case Some(token) =>
        onNext(ReqlFrame(token, command.body))

      case _ =>
        logger.log(LOGICAL_CONNECTION, self.toString(), s"Worker $commandSender already disconnected")
    }
  }

  private val sendBuffered: BufferedRenderedCommand => Unit = { buffered =>
    sendNext(buffered.command, buffered.sender)
  }

  override def preStart(): Unit = {
    logger.log(LOGICAL_CONNECTION, self.toString(), "Start done")
  }

  override def postStop(): Unit = {
    val wasActive = isActive
    val wasCanceled = canceled
    onComplete()
    cancel()
    logger.log(LOGICAL_CONNECTION, self.toString(), s"Stop done: wasActive = $wasActive, wasCanceled = $wasCanceled")
  }

  override def receive: Receive = {
    case command: RenderedCommand =>
      val commandSender = sender()

      if (command.generateToken) {
        tokenToWorker.update(tokenCounter, commandSender)
        workerToToken.update(commandSender, tokenCounter)
        tokenCounter += 1
      }

      if (totalDemand > 0) {
        sendNext(command, commandSender)
      } else {
        val buffered = BufferedRenderedCommand(command, commandSender)
        buffer += buffered
        ()
      }

    case ActorPublisherMessage.Request(n) =>
      if (totalDemand > 0) {
        logger.log(LOGICAL_CONNECTION, self.toString(), s"requested = $n; demand = $totalDemand; buffered = ${buffer.length}")
        val howMany = Math.min(totalDemand, buffer.length.toLong).toInt
        val toSend = buffer.take(howMany)
        buffer.remove(0, howMany)
        logger.log(LOGICAL_CONNECTION, self.toString(), s"new_buffer_size = ${buffer.length}; to_send = ${toSend.length}")
        toSend.foreach(sendBuffered)
      }

    case ActorSubscriberMessage.OnNext(next) =>
      next match {
        case ReqlFrame(responseToken, body) =>
          tokenToWorker.get(responseToken) match {
            case Some(worker) =>
              worker ! LogicalConnectionProtocol.RawResponse(body)

            case _ =>
              logger.log(LOGICAL_CONNECTION, self.toString(), s"Worker for token $responseToken already disconnected")
          }
      }

    case ConnectionReleased(workerRef) =>
      logger.log(LOGICAL_CONNECTION, self.toString(), s"Worker $workerRef released connection")
      workerToToken.get(workerRef) match {
        case Some(token) =>
          tokenToWorker.remove(token)
          workerToToken.remove(workerRef)
          ()

        case _ =>
      }

    case PrepareForShutdown =>
      logger.log(LOGICAL_CONNECTION, self.toString(), "Preparing for shutdown")
      //TODO: clean state???
      val initiator = sender()
      initiator ! ReadyToShutdown(self)

    case ActorPublisherMessage.Cancel =>
      logger.log(LOGICAL_CONNECTION, self.toString(), "Cancel [ignored]")
      //context.parent ! ConnectionProblem(self)

    case ActorSubscriberMessage.OnError(graphException) =>
      logger.log(LOGICAL_CONNECTION, self.toString(), s"OnError($graphException)")
      val cause = new ReqlDriverError(s"Connection closed with error: ${graphException.getMessage}")
      context.parent ! ConnectionProblem(self, cause)

    case ActorSubscriberMessage.OnComplete =>
      logger.log(LOGICAL_CONNECTION, self.toString(), "OnComplete [ignored]")
      //val cause = new ReqlDriverError("Connection closed by server")
      //context.parent ! ConnectionProblem(self, cause)

    case x =>
      logger.log(LOGICAL_CONNECTION, self.toString(), s"Unknown message: $x")
  }
}

object LogicalConnection {
  def props(logger: Logger): Props = Props(new LogicalConnection(logger))

  def makeLogicalConnection(
    parentContext: ActorRefFactory,
    name: String,
    logger: Logger
  ): Flow[ByteString, ByteString, ActorRef] = {
    val connectionRef = parentContext.actorOf(LogicalConnection.props(logger), name)
    val frameFlowSource = Source.fromPublisher[ReqlFrame](ActorPublisher(connectionRef))
    val frameFlowSink = Sink.fromSubscriber[ReqlFrame](ActorSubscriber(connectionRef))
    val frameFlow = Flow.fromSinkAndSourceMat(frameFlowSink, frameFlowSource)(Keep.right)

    frameFlow.joinMat(ReqlFraming.framingBidi)((_, _) => connectionRef)
  }
}

object LogicalConnectionProtocol {
  final case class RenderedCommand(body: ByteString, generateToken: Boolean)
  final case class RawResponse(body: ByteString)

  final case class ConnectionReleased(workerRef: ActorRef)

  final case object PrepareForShutdown
  final case class ReadyToShutdown(connectionRef: ActorRef)
}