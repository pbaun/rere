package rere.driver.pool.impl

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.locks.ReentrantLock

import rere.driver.connection.Connection

import scala.util.Random

trait ConnectionBag[T <: Connection] {
  def selectConnection(): T
  def addConnection(): T
  def replaceConnection(oldConnection: T): Boolean
  def removeConnection(toRemove: T): Boolean
  def clear(): Seq[T]
}

final class P2CCopyOnWriteBag[T <: Connection](
    maxSize: Int,
    mkConnection: Long => T
  ) extends ConnectionBag[T] {

  private val connectionsCounter = new AtomicLong(0L)
  private val lock = new ReentrantLock()

  @volatile private var data: Vector[T] = {
    val builder = Vector.newBuilder[T]
    (0 until maxSize).foreach { _ =>
      builder += mkConnection(connectionsCounter.getAndIncrement())
    }
    builder.result()
  }

  override def selectConnection(): T = {
    val snapshot = data
    if (snapshot.isEmpty) {
      throw new IllegalStateException("Bag is empty")
    }
    val length = snapshot.length
    val randomIndex = Random.nextInt(length)
    val connection1 = snapshot(randomIndex)
    val connection2 = snapshot((randomIndex + 1) % length)
    if (connection1.load < connection2.load) connection1 else connection2
  }

  def addConnection(): T = {
    if (data.size >= maxSize) {
      throw new IllegalStateException("Bag is full")
    } else {
      lock.lock()
      try {
        val snapshot = data
        if (snapshot.size >= maxSize) {
          throw new IllegalStateException("Bag is full")
        } else {
          val newConnection = mkConnection(connectionsCounter.getAndIncrement())
          data = snapshot :+ newConnection
          newConnection
        }
      } finally {
        lock.unlock()
      }
    }
  }

  override def replaceConnection(oldConnection: T): Boolean = {
    lock.lock()
    try {
      val snapshot = data
      val oldConnectionIndex = snapshot.indexOf(oldConnection)
      if (oldConnectionIndex == -1) {
        false
      } else {
        data = snapshot.updated(oldConnectionIndex, mkConnection(connectionsCounter.getAndIncrement()))
        true
      }
    } finally {
      lock.unlock()
    }
  }

  def removeConnection(toRemove: T): Boolean = {
    lock.lock()
    try {
      val snapshot = data
      data = snapshot.filter(_ != toRemove)
      data.size < snapshot.size
    } finally {
      lock.unlock()
    }
  }

  override def clear(): Seq[T] = {
    lock.lock()
    try {
      val snapshot = data
      data = Vector.empty[T]
      snapshot
    } finally {
      lock.unlock()
    }
  }
}
