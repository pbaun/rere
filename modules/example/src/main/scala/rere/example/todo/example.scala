package rere.example.todo

import java.time.ZonedDateTime
import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import rere.driver.pool.ConnectionPool
import rere.driver.{ConnectionSettings, Credentials}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

object Example {

  def main(args: Array[String]): Unit = {
    implicit val executor = ExecutionContext.global
    implicit val system = ActorSystem("todo-example")

    val credentials = Credentials("admin", "")
    val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)
    val pool = ConnectionPool.create(credentials, settings, "todo-example-pool", 1)

    val service = new TaskService(pool)


    val changefeedDone = service.subscribeToChanges(Sink.foreach { notification =>
      println(s"-- Task is changed: old value = ${notification.oldVal}; new value = ${notification.newVal}")
    })

    val queryDone = service.list().flatMap { tasks =>
      println(s"Current tasks: $tasks")

      val newTask = Task(UUID.randomUUID(), UUID.randomUUID(), "test task", Seq("test", "task"),
        reminder = Reminder(Some(ZonedDateTime.now().plusHours(2)), repeat = false, None)
      )

      service.create(newTask).flatMap {
        case Some(generatedId) =>
          println(s"Task $generatedId was created")

          service.list().flatMap { tasks =>
            println(s"Current tasks: $tasks")

            service.delete(generatedId).map { deleted =>
              println(s"Deleted $deleted tasks")
            }
          }

        case _ =>
          Future.failed(new Exception("Task was not created"))
      }
    }

    val shutdownFuture = for {
      _ <- queryDone
      _ <- pool.shutdown()
      _ <- changefeedDone
      _ <- system.terminate()
    } yield ()

    Await.result(shutdownFuture, 20.seconds)
  }

}
