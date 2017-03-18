package rere.example.todo

import java.util.UUID

import akka.stream.scaladsl.Sink
import rere.driver.pool.ConnectionPool

import scala.concurrent.{ExecutionContext, Future}

class TaskService(pool: ConnectionPool)(implicit executor: ExecutionContext) {

  import TodoDatabase.tasks
  import rere.driver.runners.all._
  import rere.ql.queries.all._

  def list(): Future[Seq[Task]] = {
    val (seq, _) = tasks.table().run(pool).drainTo(Sink.seq[Task])
    seq.flatten
  }

  def create(task: Task): Future[Long] = {
    tasks.table().insert(task).run(pool).future().map { result =>
      result.inserted
    }
  }

  def delete(uuid: UUID): Future[Long] = {
    tasks.table().get(uuid.toString).delete().run(pool).future().map { result =>
      result.deleted
    }
  }

}
