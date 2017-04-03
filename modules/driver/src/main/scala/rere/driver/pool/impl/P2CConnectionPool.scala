package rere.driver.pool.impl

import akka.Done
import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Tcp.OutgoingConnection
import rere.driver.connection.LogicalConnectionProtocol
import rere.driver.exceptions.ReqlDriverError
import rere.driver.logger.{Logger, POOL}
import rere.driver.pool._
import rere.driver.workers.{HeartbeatWorker, QueryWorkerProtocol, WorkerContext}
import rere.driver.{ConnectionSettings, Credentials}
import rere.sasl.scram.cache.SaltedPasswordCache
import rere.sasl.scram.crypto.entropy.EntropySource
import rere.sasl.scram.crypto.entropy.impl.SecureEntropySource

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Random, Success, Try}

/**
  * The power of two random choices [https://www.eecs.harvard.edu/~michaelm/postscripts/mythesis.pdf]
  */
class P2CConnectionPool(actorRef: ActorRef, logger: Logger) extends ConnectionPool {
  override def send(message: ConnectionPoolIncomingMessages): Unit = {
    actorRef ! message
  }

  override def shutdown(): Future[ConnectionPoolShutdownResult] = {
    val whenShutdown = Promise[ConnectionPoolShutdownResult]()
    actorRef ! Shutdown(whenShutdown)
    logger.log(POOL, actorRef.toString(), "Connection pool shutdown start")
    whenShutdown.future
  }
}

class P2CConnectionPoolActor(
    credentials: Credentials,
    connectionSettings: ConnectionSettings,
    poolSize: Int,
    reconnectTimeout: FiniteDuration,
    logger: Logger
  ) extends Actor with ConnectionCreator {

  implicit val actorSystem = context.system
  implicit val materializer = ActorMaterializer(
    //ActorMaterializerSettings.create(actorSystem).withInputBuffer(1, 1)
    //TODO: Materializer should have buffer and because of that all sources are prefetched.
    //      For example, logical connection receives demand request before auth commander completes
    //      auth process and switcher starts demand data from logical connection.
  )

  private val entropySource: EntropySource = SecureEntropySource
  private val saltedPasswordCache: SaltedPasswordCache = SaltedPasswordCache()

  private var connectionIdCounter: Long = 0L
  private var requestCounter: Long = 0L
  private var isInShutdownPhase = false
  private val shutdownPromise = Promise[ConnectionPoolShutdownResult]()
  private var connectionCreateDelay: FiniteDuration = Duration.Zero

  private val poolState: mutable.HashMap[ActorRef, ConnectionState] = mutable.HashMap.empty
  private val readySubset: mutable.ArrayBuffer[ActorRef] = mutable.ArrayBuffer.empty
  private val poisonedButNotDisconnected: mutable.ArrayBuffer[ActorRef] = mutable.ArrayBuffer.empty

  case class ConnectionState(
    var physicalConnectionStatus: PhysicalConnectionStatus,
    var lifecyclePhase: LifecyclePhase,
    workers: mutable.ArrayBuffer[ActorRef])

  sealed trait LifecyclePhase
  case object Active extends LifecyclePhase
  case object RequestedForShutdown extends LifecyclePhase
  case object ReadyToShutdown extends LifecyclePhase
  case object Poisoned extends LifecyclePhase

  sealed trait PhysicalConnectionStatus
  case object Connected extends PhysicalConnectionStatus
  case object Disconnected extends PhysicalConnectionStatus

  case object Start
  case object CreateConnection
  case class Connected(outgoingConnection: OutgoingConnection)
  case class ServerUnreachable(connectionRef: ActorRef, transportException: Throwable)
  case class ConnectionGraphDone(connectionRef: ActorRef, result: Try[Done])

  override def preStart(): Unit = {
    self ! Start
  }

  override def postStop(): Unit = {
    logger.log(POOL, self.toString(), poolState.toString())
    logger.log(POOL, self.toString(), "Stop done")
  }

  override def receive: Receive = {
    case Start =>
      (0 until poolSize).foreach { _ =>
        createOneConnection()
      }

    case CreateConnection =>
      if (!isInShutdownPhase && readySubset.length != poolSize) {
        createOneConnection()
      }

    case msg: AcquireConnection =>
      val readyConnections = readySubset.length
      if (readyConnections == 0) {
        msg.onConnectionFail(new ReqlDriverError("No connections left in the pool."))
        logger.log(POOL, self.toString(), "No ready connection left")
      } else {
        val firstRef = readySubset(Random.nextInt(readyConnections))
        val secondRef = readySubset(Random.nextInt(readyConnections))

        val firstState = poolState(firstRef)
        val secondState = poolState(secondRef)
        val chosenRef = if (firstState.workers.length < secondState.workers.length) firstRef else secondRef

        val workerContext = WorkerContext(materializer, chosenRef, logger)
        val workerRef = context.actorOf(msg.workerProps(workerContext), s"query_worker-$requestCounter")
        requestCounter += 1
        poolState(chosenRef).workers += workerRef
        logger.log(POOL, self.toString(), s"Connection $chosenRef is acquired")
      }

    case Shutdown(whenShutdown) =>
      if (!isInShutdownPhase) {
        isInShutdownPhase = true

        poolState.foreach { case (connectionRef, state) =>
          if (state.lifecyclePhase == Active) {

            connectionRef ! LogicalConnectionProtocol.PrepareForShutdown
            state.lifecyclePhase = RequestedForShutdown
            readySubset -= connectionRef

            state.workers.foreach { worker =>
              worker ! QueryWorkerProtocol.ShutdownNow
            }
          }
        }
      }
      whenShutdown.completeWith(shutdownPromise.future)
      tryShutdownPool()   // maybe pool is empty

    case LogicalConnectionProtocol.ReadyToShutdown(connectionRef) =>
      val state = poolState(connectionRef)
      state.lifecyclePhase = ReadyToShutdown
      tryPoisonSingleConnection(connectionRef, state)
      tryShutdownPool()

    case QueryWorkerProtocol.ShutdownComplete(connectionRef, workerRef) =>
      logger.log(POOL, self.toString(), s"ShutdownComplete($connectionRef, $workerRef)")
      val state = poolState(connectionRef)
      state.workers -= workerRef
      tryPoisonSingleConnection(connectionRef, state)
      tryShutdownPool()

    case Connected(connection) =>
      logger.log(POOL, self.toString(), s"Connected($connection)")
      resetConnectionCreateDelay()

    case ConnectionProblem(connectionRef, cause) =>
      logger.log(POOL, self.toString(), s"Problem with connection $connectionRef $cause")
      tryToStartConnectionShutdown(connectionRef, cause)
      increaseConnectionCreateDelay()

    case ServerUnreachable(connectionRef, transportException) =>
      logger.log(POOL, self.toString(), s"Server unreachable $transportException on connection $connectionRef")
      val cause = new ReqlDriverError(s"Server unreachable: ${transportException.getMessage}")
      tryToStartConnectionShutdown(connectionRef, cause)
      increaseConnectionCreateDelay()

    case ConnectionGraphDone(connectionRef, result) =>
      logger.log(POOL, self.toString(), s"Graph of $connectionRef done: $result")
      markConnectionAsDisconnected(connectionRef)
      tryShutdownPool()
  }

  private def createOneConnection(): Unit = {
    val connectionId = connectionIdCounter
    connectionIdCounter += 1

    val connectionDescriptor =
      makeOneConnection(connectionId, credentials, connectionSettings, entropySource, saltedPasswordCache, logger)
    val heartbeatWorkerRef =
      context.actorOf(
        HeartbeatWorker.props(
          firstCheckDelay = connectionSettings.firstCheckDelay,
          checkInterval = connectionSettings.checkInterval,
          connectionDescriptor.connectionRef,
          logger),
        s"heartbeat_worker-$connectionId")

    import context.dispatcher

    connectionDescriptor.connectionFuture.onComplete {
      case Success(res) =>
        self ! Connected(res)

      case Failure(cause) =>
        self ! ServerUnreachable(connectionDescriptor.connectionRef, cause)
    }

    connectionDescriptor.connectionDoneFuture.onComplete { result =>
      self ! ConnectionGraphDone(connectionDescriptor.connectionRef, result)
    }

    poolState.put(
      connectionDescriptor.connectionRef,
      ConnectionState(Connected, Active, mutable.ArrayBuffer(heartbeatWorkerRef))
    )
    readySubset += connectionDescriptor.connectionRef
    ()
  }

  private def refillPool(): Unit = {
    if (!isInShutdownPhase) {
      if (connectionCreateDelay == Duration.Zero) {
        createOneConnection()
      } else {
        implicit val ec = context.dispatcher
        //TODO: collect Cancellables and cancel right after shutdown start
        context.system.scheduler.scheduleOnce(connectionCreateDelay, self, CreateConnection)
        ()
      }
    }
  }

  private def increaseConnectionCreateDelay(): Unit = {
    if (connectionCreateDelay == Duration.Zero) {
      connectionCreateDelay = reconnectTimeout
    } else {
      //TODO: use strategy?
      if (connectionCreateDelay < 1.minute) {
        connectionCreateDelay *= 2
      } else {
        connectionCreateDelay = 2.minutes
      }
    }
  }

  private def resetConnectionCreateDelay(): Unit = {
    if (connectionCreateDelay != Duration.Zero) {
      logger.log(POOL, self.toString(), s"Resetting connection delay from $connectionCreateDelay to 0")
      connectionCreateDelay = Duration.Zero
    }
  }

  private def tryToStartConnectionShutdown(connectionRef: ActorRef, cause: Throwable): Unit = {
    val state = poolState(connectionRef)

    // no reason to start shutdown if state is not READY. it means that shutdown already started
    if (state.lifecyclePhase == Active) {
      connectionRef ! LogicalConnectionProtocol.PrepareForShutdown
      state.lifecyclePhase = RequestedForShutdown
      state.workers.foreach { workerRef =>
        workerRef ! QueryWorkerProtocol.ConnectionLost(cause)
      }

      readySubset -= connectionRef
      refillPool()
    }
  }

  private def tryPoisonSingleConnection(connectionRef: ActorRef, state: ConnectionState): Unit = {
    logger.log(POOL, self.toString(), s"tryPoisonSingleConnection($connectionRef); $state")

    if (state.workers.isEmpty && state.lifecyclePhase == ReadyToShutdown) {
      connectionRef ! PoisonPill
      state.lifecyclePhase = Poisoned
      if (state.physicalConnectionStatus != Disconnected) {
        poisonedButNotDisconnected += connectionRef
        ()
      }
    }
  }

  private def markConnectionAsDisconnected(connectionRef: ActorRef): Unit = {
    val state = poolState(connectionRef)
    state.physicalConnectionStatus = Disconnected
    if (state.lifecyclePhase == Poisoned) {
      poisonedButNotDisconnected -= connectionRef
    }

    readySubset -= connectionRef
    refillPool()
  }

  private def isAllConnectionsClosed: Boolean = {
    poolState.forall { case (_, state) =>
      state.lifecyclePhase == Poisoned && state.physicalConnectionStatus == Disconnected
    }
  }

  private def tryShutdownPool(): Unit = {
    if (isInShutdownPhase &&
        readySubset.isEmpty &&
        poisonedButNotDisconnected.isEmpty &&
        isAllConnectionsClosed) {

      val msg = ShutdownSuccessfullyDone(
        queriesExecuted = requestCounter,
        connectionsTurnedOff = poolState.size
      )
      shutdownPromise.trySuccess(msg)
      context.stop(self)

      logger.log(POOL, self.toString(), "Connection pool shutdown done")
    }
  }
}

object P2CConnectionPoolActor {
  def props(
    credentials: Credentials,
    connectionSettings: ConnectionSettings,
    poolSize: Int,
    reconnectTimeout: FiniteDuration,
    logger: Logger
  ): Props = {
    Props(new P2CConnectionPoolActor(credentials, connectionSettings, poolSize, reconnectTimeout, logger))
  }
}