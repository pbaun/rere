package rere.driver.runners.ready

import scala.concurrent.Future

trait SingleValueReadyToGo[Out] {
  def future(): Future[Out]
}
