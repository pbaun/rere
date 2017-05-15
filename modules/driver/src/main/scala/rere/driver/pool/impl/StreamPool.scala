package rere.driver.pool.impl

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import akka.Done
import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.stream.scaladsl.{GraphDSL, Keep, RunnableGraph, Sink}
import akka.stream.{ActorMaterializer, ClosedShape, Materializer}
import rere.driver.auth.AuthCommander
import rere.driver.connection._
import rere.driver.pool._
import rere.driver.util.StreamsDebugging
import rere.driver.workers.QueryWorkerProtocol.ConnectionLost
import rere.driver.workers.{AtomQueryWorker, HeartbeatWorker, StreamQueryWorker}
import rere.driver.{ConnectionSettings, Credentials}
import rere.ql.options.Options
import rere.ql.types.ReqlExpr
import rere.ql.wire.ReqlDecoder
import rere.sasl.scram.cache.SaltedPasswordCache
import rere.sasl.scram.client.SCRAMClient
import rere.sasl.scram.crypto.ErrorReporter
import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.scram.crypto.entropy.impl.SecureEntropySource
import rere.sasl.scram.crypto.sha256.ScramSha256AuthMechanismFactory

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

final class StreamPool(
    credentials: Credentials,
    connectionSettings: ConnectionSettings,
    poolSettings: ConnectionPoolSettings,
    actorSystem: ActorSystem
  ) extends ConnectionPool {

  private val started = new AtomicBoolean(true)
  private val materializer = ActorMaterializer(namePrefix = Some(poolSettings.poolName))(actorSystem)
  private val logger = Logging(actorSystem, s"${poolSettings.poolName}-pool")

  private val workersCounter = new AtomicLong(0L)

  private val entropySource: EntropySource = SecureEntropySource
  private val saltedPasswordCache: SaltedPasswordCache = SaltedPasswordCache()

  private val backoff = new LinearBackoffStrategy(poolSettings.backoffStep, poolSettings.backoffMaxSteps)
  private val connectionBag = new P2CCopyOnWriteBag[LogicalConnection](poolSettings.poolSize, { connectionId: Long =>
    createOneConnection(connectionId, actorSystem, materializer)
  })

  private def createOneConnection(
    connectionId: Long,
    actorSystem: ActorSystem,
    materializer: Materializer
  ): LogicalConnection = {
    val connectionLogger = Logging(actorSystem, s"connection-$connectionId")

    val watchedTcpConnection = TCPTransport
      .makeConnection(connectionSettings)(actorSystem)
      .join(StreamsDebugging.loggingBidi(connectionLogger))

    val commander = {
      val errorReporter = new ErrorReporter {
        override def onError(throwable: Throwable): Unit = {
          connectionLogger.error(throwable, "Cryptographic error")
        }
      }
      val mechanism = ScramSha256AuthMechanismFactory.getMechanism(errorReporter)

      AuthCommander.getCommanderFlow(
        new AuthCommander(
          SCRAMClient(mechanism, entropySource, saltedPasswordCache),
          credentials.login,
          credentials.password,
          connectionLogger
        )
      )
    }

    val realConnectionWithFraming = ReqlFraming.framingBidi.joinMat(
      Switcher.toDataFlow(watchedTcpConnection, commander, connectionLogger)
    )(Keep.right)

    val (realConnectedF, logicalConnection) =
      LogicalConnection.connect(connectionId, realConnectionWithFraming, connectionLogger)(materializer)

    val connectionLostPromise = Promise[ConnectionLost]()

    realConnectedF.onComplete {
      case Success(_) =>
        backoff.resetTimeout()
        val heartbeatLogger = Logging(actorSystem, s"connection-checker-$connectionId")
        val heartbeatWorkerDone = spawnHeartbeatWorker(
          logicalConnection,
          connectionLostPromise,
          heartbeatLogger
        )

      case Failure(ex) =>
        backoff.increaseTimeout()
        connectionLostPromise.tryFailure(ex)
    }(actorSystem.dispatcher)

    connectionLostPromise.future.onComplete { _ =>
      logger.info("connection #{} is lost and will be removed from pool", connectionId)
      if (connectionBag.removeConnection(logicalConnection)) {
        logicalConnection.prepareForShutdown().map { _ =>
          val delay = backoff.getTimeout()
          actorSystem.scheduler.scheduleOnce(delay, new Runnable {
            override def run(): Unit = {
              if (started.get()) {
                connectionBag.addConnection()
              }
              ()
            }
          })(actorSystem.dispatcher)
          logger.info("creation of replacement for #{} will be postponed for {}", connectionId, delay)
        }(actorSystem.dispatcher)
      }
    }(actorSystem.dispatcher)

    logicalConnection
  }

  private def spawnHeartbeatWorker(
    connection: LogicalConnection,
    connectionLostPromise: Promise[ConnectionLost],
    connectionLogger: LoggingAdapter
  ): Future[Done] = {
    val connectionFlow = connection.getConnectionFlow()

    val heartbeatWorker = new HeartbeatWorker(
      poolSettings.heartbeatInitialDelay, poolSettings.heartbeatCheckInterval,
      connectionLostPromise, connectionLogger)

    val graph = RunnableGraph.fromGraph(GraphDSL.create(
      heartbeatWorker, connectionFlow) {
      (workerFlowM, connectionFlowM) => workerFlowM
    } { implicit builder =>
      (workerFlow, connectionFlow) =>

        import GraphDSL.Implicits._

        workerFlow.commands ~> connectionFlow ~> workerFlow.dbResponses

        ClosedShape
    })

    val workerDone = graph.run()(materializer)

    workerDone
  }

  override def runAtom[Expr <: ReqlExpr, Out](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out]
  ): Future[Out] = {
    if (!started.get()) {
      throw new IllegalStateException("Pool is not started")
    }
    val workerId = workersCounter.getAndIncrement()
    val workerLogger = Logging(actorSystem, s"worker-$workerId")
    val workerFlow = new AtomQueryWorker(expr, runOptions, reqlDecoder, workerLogger)

    val connection = connectionBag.selectConnection()
    logger.debug("connection #{} is selected for worker #{}", connection.id, workerId)
    val connectionFlow = connection.getConnectionFlow()

    val graph = RunnableGraph.fromGraph(GraphDSL.create(
      workerFlow, connectionFlow) {
      (workerFlowM, connectionFlowM) => workerFlowM
    } { implicit builder =>
      (workerFlow, connectionFlow) =>

        import GraphDSL.Implicits._

        workerFlow.commands ~> connectionFlow ~> workerFlow.dbResponses

        ClosedShape
    })

    graph.run()(materializer)
  }

  override def runStream[Expr <: ReqlExpr, Out, OutMat](
    expr: Expr,
    runOptions: Options,
    reqlDecoder: ReqlDecoder[Out],
    outSink: Sink[Out, OutMat]
  ): OutMat = {
    if (!started.get()) {
      throw new IllegalStateException("Pool is not started")
    }
    val workerId = workersCounter.getAndIncrement()
    val workerLogger = Logging(actorSystem, s"worker-$workerId")
    val workerFlow = new StreamQueryWorker(expr, runOptions, reqlDecoder, workerLogger)

    val connection = connectionBag.selectConnection()
    logger.debug("connection #{} is selected for worker #{}", connection.id, workerId)
    val connectionFlow = connection.getConnectionFlow()

    val graph = RunnableGraph.fromGraph(GraphDSL.create(
      workerFlow, outSink, connectionFlow) {
      (workerFlowM, outSinkM, connectionFlowM) => outSinkM
    } { implicit builder =>
      (workerFlow, outSink, connectionFlow) =>

        import GraphDSL.Implicits._

        workerFlow.commands ~> connectionFlow ~> workerFlow.dbResponses

        workerFlow.out      ~> outSink.in

        ClosedShape
    })

    graph.run()(materializer)
  }

  override def shutdown(): Future[Done] = {
    started.set(false)
    implicit val executionContext = actorSystem.dispatcher
    Future.sequence(
      connectionBag.clear().map(_.prepareForShutdown())
    ).map { _ =>
      materializer.shutdown()
      Done
    }
  }
}
