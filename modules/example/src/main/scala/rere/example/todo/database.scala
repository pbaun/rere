package rere.example.todo

import rere.ql.shapes.DatabaseShape

object TodoDatabase extends DatabaseShape("todo") {
  implicit val tasks = table[Task]("tasks", TaskShape)
}
