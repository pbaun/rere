package rere.example.todo

import java.util.UUID

import akka.stream.scaladsl.Sink
import rere.driver.pool.ConnectionPool
import rere.ql.data.ChangefeedNotification

import scala.concurrent.{ExecutionContext, Future}

class TaskService(pool: ConnectionPool)(implicit executor: ExecutionContext) {

  import TodoDatabase.tasks
  import rere.driver.runners.all._
  import rere.ql.queries.all._

  def list(): Future[Seq[Task]] = {
    val (seq, _) = tasks.table().run(pool).drainTo(Sink.seq[Task])
    seq.flatMap(identity)
  }

  def create(task: Task): Future[Option[UUID]] = {
    tasks.table().insertAuto(task).run(pool).future().map { result =>
      result.generatedKeys.flatMap(_.headOption)
    }
  }

  def delete(uuid: UUID): Future[Long] = {
    tasks.table().get(uuid).delete().run(pool).future().map { result =>
      result.deleted
    }
  }

  def subscribeToChanges[Mat](sink: Sink[ChangefeedNotification[Task], Mat]): Future[Mat] = {
    val (mat, _) = tasks.table().changes().run(pool).drainTo(sink)
    mat
  }

}
