package rere.driver.pool.impl

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.FiniteDuration

trait BackoffStrategy {
  def increaseTimeout(): Unit
  def resetTimeout(): Unit
  def getTimeout(): FiniteDuration
}

class LinearBackoffStrategy(oneStep: FiniteDuration, maxSteps: Long) extends BackoffStrategy {
  private val backoffCounter = new AtomicLong(0L)

  override def increaseTimeout(): Unit = {
    backoffCounter.getAndIncrement()
    ()
  }

  override def resetTimeout(): Unit = {
    backoffCounter.set(0L)
  }

  override def getTimeout(): FiniteDuration = {
    oneStep * Math.min(backoffCounter.get(), maxSteps)
  }
}
