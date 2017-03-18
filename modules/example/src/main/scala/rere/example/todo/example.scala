package rere.example.todo

import java.time.ZonedDateTime
import java.util.UUID

import akka.actor.ActorSystem
import rere.driver.pool.ConnectionPool
import rere.driver.{ConnectionSettings, Credentials}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

object Example {

  def main(args: Array[String]): Unit = {
    implicit val executor = ExecutionContext.global
    implicit val system = ActorSystem("todo-example")

    val credentials = Credentials("admin", "")
    val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)
    val pool = ConnectionPool.create(credentials, settings, "todo-example-pool", 1)

    val service = new TaskService(pool)

    val queryDone = service.list().flatMap { tasks =>
      println(s"Current tasks: $tasks")

      val newTask = Task(UUID.randomUUID(), UUID.randomUUID(), "test task", Seq("test", "task"),
        reminder = Reminder(Some(ZonedDateTime.now().plusHours(2)), repeat = false, None)
      )

      service.create(newTask).flatMap { created =>
        println(s"Created $created tasks")

        service.list().flatMap { tasks =>
          println(s"Current tasks: $tasks")

          service.delete(newTask.uuid).map { deleted =>
            println(s"Deleted $deleted tasks")
          }
        }
      }
    }

    val shutdownFuture = for {
      _ <- queryDone
      _ <- pool.shutdown()
      _ <- system.terminate()
    } yield ()

    Await.result(shutdownFuture, 20.seconds)
  }

}
