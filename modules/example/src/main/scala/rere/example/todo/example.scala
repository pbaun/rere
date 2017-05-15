package rere.example.todo

import java.time.ZonedDateTime
import java.util.UUID

import akka.Done
import akka.pattern.after
import akka.actor.ActorSystem
import akka.stream.KillSwitches
import akka.stream.scaladsl.{Flow, Keep, Sink}
import com.typesafe.config.ConfigFactory
import rere.driver.pool.ConnectionPool
import rere.driver.{ConnectionSettings, Credentials}
import rere.ql.data.ChangefeedNotification

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

object Example {

  def main(args: Array[String]): Unit = {
    implicit val executor = ExecutionContext.global
    val config = ConfigFactory.parseString("akka.loglevel=DEBUG")
    implicit val system = ActorSystem("todo-example", config)

    val credentials = Credentials("admin", "")
    val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)
    val pool = ConnectionPool.create(credentials, settings, "todo-example-pool", 1)

    val service = new TaskService(pool)


    val changefeedSink = Flow.fromGraph(KillSwitches.single[ChangefeedNotification[Task]])
      .via(Flow.fromFunction({ notification: ChangefeedNotification[Task] =>
        println(s"-- Task is changed: old value = ${notification.oldVal}; new value = ${notification.newVal}")
        notification
      }))
      .toMat(Sink.seq)(Keep.both)

    val (killSwitch, changefeedDone) = service.subscribeToChanges(changefeedSink)

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
      _ <- after(3.seconds, system.scheduler)(Future.successful(Done))
      _ = killSwitch.shutdown()
      _ <- changefeedDone
      _ <- pool.shutdown()
      _ <- system.terminate()
    } yield ()

    Await.result(shutdownFuture, 20.seconds)
    ()
  }

}
