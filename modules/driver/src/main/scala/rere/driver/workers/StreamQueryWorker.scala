package rere.driver.workers

import akka.Done
import akka.actor.{Actor, ActorRef, Props}
import akka.stream.ActorMaterializer
import akka.stream.actor.{ActorPublisher, ActorPublisherMessage}
import akka.stream.scaladsl.{Keep, Sink, Source}
import io.circe.Json
import rere.driver.connection.LogicalConnectionProtocol
import rere.driver.exceptions.{ReqlDecodeError, ReqlDriverError, ReqlQueryFailedException, ReqlQueryShutdownException}
import rere.driver.logger.{Logger, STREAM_WORKER}
import rere.driver.protocol._
import rere.ql.options.Options
import rere.ql.ql2.Response.ResponseType
import rere.ql.types.ReqlExpr
import rere.ql.wire.ReqlDecoder

import scala.concurrent.Promise
import scala.util.Success

class StreamQueryWorker[Expr <: ReqlExpr, Out, Mat](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out],
    sink: Sink[Out, Mat],
    matPromise: Promise[Mat],
    queryDonePromise: Promise[Done],
    actorMaterializer: ActorMaterializer,
    connectionRef: ActorRef,
    logger: Logger
  ) extends Actor with ActorPublisher[Out] {

  private implicit val materializer = actorMaterializer
  private implicit val ec = context.dispatcher
  private val source = Source.fromPublisher(ActorPublisher[Out](self))

  var responseBuffer = Vector.empty[Out]

  var waitingForResponse = false
  var isTerminating = false

  override def preStart(): Unit = {
    logger.log(STREAM_WORKER, self.toString(), "Start done")
    val (sourceDoneFuture, mat) = source.watchTermination()(Keep.right).toMat(sink)(Keep.both).run()
    queryDonePromise.completeWith(sourceDoneFuture)
    matPromise.tryComplete(Success(mat))

    if (!isCanceled) {
      val command = new ReqlStartCommand[Expr](expr, runOptions)
      connectionRef ! LogicalConnectionProtocol.RenderedCommand(ReqlCommand.render(command), generateToken = true)
      waitingForResponse = true
      tryToPushResponse()
      tryToRequestMore()
    } else {
      tryToStopSelf()
    }
  }

  override def postStop(): Unit = {
    logger.log(STREAM_WORKER, self.toString(), "postStop called")
    val parent = context.parent
    val workerRef = self
    queryDonePromise.future.onComplete { _ =>
      parent ! QueryWorkerProtocol.ShutdownComplete(connectionRef, workerRef)
      logger.log(STREAM_WORKER, workerRef.toString(), "Stop done")
    }
  }

  private val sendBuffered: Out => Unit = { data =>
    onNext(data)
  }

  private def tryToPushResponse(): Unit = {
    if (isActive && (totalDemand > 0)) {
      val d = totalDemand
      val l = responseBuffer.length.toLong
      val howMany = Math.min(totalDemand, responseBuffer.length.toLong).toInt
      val toPush = responseBuffer.take(howMany)
      responseBuffer = responseBuffer.drop(howMany)
      toPush.foreach(sendBuffered)
      logger.log(STREAM_WORKER, self.toString(), s"pushed = ${toPush.size}; demand = $d; length = $l")

      if (isTerminating && responseBuffer.isEmpty) {
        onCompleteThenStop()
        logger.log(STREAM_WORKER, self.toString(), "isTerminating && buffer.isEmpty")
      }
    }
  }

  private def tryToRequestMore(): Unit = {
    if (!waitingForResponse && (totalDemand > 0) && !isTerminating) {
      val command = new ReqlContinueCommand
      connectionRef ! LogicalConnectionProtocol.RenderedCommand(ReqlCommand.render(command), generateToken = false)
      logger.log(STREAM_WORKER, self.toString(), "requested more")
      waitingForResponse = true
    }
  }

  private def tryToStopStream(): Unit = {
    if (isTerminating) {
      onCompleteThenStop()
      context.stop(self)
    } else {
      if (!waitingForResponse) {
        val command = new ReqlStopCommand
        connectionRef ! LogicalConnectionProtocol.RenderedCommand(ReqlCommand.render(command), generateToken = false)
        logger.log(STREAM_WORKER, self.toString(), "stream stopped")
        waitingForResponse = true
      } else {
        logger.log(STREAM_WORKER, self.toString(), "don't know what to do; waiting for response")
      }
    }
  }

  private def tryToStopSelf(): Unit = {
    if (isCanceled) {
      context.stop(self)
    }
  }

  private def tryToDecodeAndBuffer(responseData: List[Json]): Unit = {
    val decodedData: List[Out] = responseData.map { element =>
      reqlDecoder.decode(element) match {
        case Right(out) =>
          out
        case Left(error) =>
          onErrorThenStop(new ReqlDecodeError(
            s"Can't decode stream element. Query will be finished with error. $error"
          ))
          context.stop(self)
          return
      }
    }

    responseBuffer = responseBuffer ++ decodedData
  }

  private def tryToCancelWithError(cause: Throwable): Unit = {
    if (isActive) {
      queryDonePromise.tryFailure(cause)
      onErrorThenStop(cause)
    } else {
      context.stop(self)
    }
  }

  override def receive: Receive = {
    case ActorPublisherMessage.Request(n) =>
      logger.log(STREAM_WORKER, self.toString(), s"Request of $n (totalDemand: $totalDemand) (waitingForResponse: $waitingForResponse) (isTerminating: $isTerminating)")
      tryToPushResponse()
      tryToRequestMore()

    case rawResponse: LogicalConnectionProtocol.RawResponse =>
      waitingForResponse = false
      ReqlResponse.decode(rawResponse.body) match {
        case Left(failure) =>
          tryToCancelWithError(new ReqlDecodeError(
            s"Can't decode response. Query will be finished with error. $failure"
          ))

        case Right(response) =>
          logger.log(STREAM_WORKER, self.toString(), s"ReqlResponse ${response.responseType}")

          response.responseType match {
            case ResponseType.SUCCESS_ATOM =>
              tryToCancelWithError(new ReqlDriverError(
                "Single value received but sequence expected"
              ))

            case ResponseType.SUCCESS_SEQUENCE =>
              logger.log(STREAM_WORKER, self.toString(), s"data.size ${response.data.length}")

              // decode and finish
              isTerminating = true
              tryToDecodeAndBuffer(response.data)
              tryToPushResponse()
              tryToStopSelf()

            case ResponseType.SUCCESS_PARTIAL =>
              logger.log(STREAM_WORKER, self.toString(), s"data.size ${response.data.length}")

              // decode and request more
              tryToDecodeAndBuffer(response.data)
              tryToPushResponse()
              tryToRequestMore()
              tryToStopSelf()

            case ResponseType.WAIT_COMPLETE =>
              tryToCancelWithError(new ReqlDriverError(
                "Queries with noReply option are not supported by driver."
              ))

            case _ =>
              val cause = new ReqlQueryFailedException(ReqlResponse.determineError(response))
              tryToCancelWithError(cause)
          }
      }

    case ActorPublisherMessage.Cancel =>
      logger.log(STREAM_WORKER, self.toString(), "Subscription canceled")
      tryToStopStream()

    case ActorPublisherMessage.SubscriptionTimeoutExceeded =>
      logger.log(STREAM_WORKER, self.toString(), "Subscription timeout exceeded")
      tryToStopStream()

    case QueryWorkerProtocol.ShutdownNow =>
      logger.log(STREAM_WORKER, self.toString(), "Will shutdown now")
      val cause = new ReqlQueryShutdownException()
      tryToCancelWithError(cause)

    case QueryWorkerProtocol.ConnectionLost(cause) =>
      logger.log(STREAM_WORKER, self.toString(), s"Connection lost: $cause")
      //TODO: detect error or not by current state. maybe it can be safely handled without error raising
      tryToCancelWithError(cause)

    case x =>
      logger.log(STREAM_WORKER, self.toString(), s"unknown message $x")
  }

}

object StreamQueryWorker {
  def props[Expr <: ReqlExpr, Out, Mat](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out],
    sink: Sink[Out, Mat],
    matPromise: Promise[Mat],
    queryDonePromise: Promise[Done],
    materializer: ActorMaterializer,
    connectionRef: ActorRef,
    logger: Logger
  ): Props = {
    Props(new StreamQueryWorker(expr, runOptions, reqlDecoder, sink, matPromise, queryDonePromise, materializer, connectionRef, logger))
  }
}
