package rere.example.todo

import java.time.ZonedDateTime
import java.util.UUID

case class Task(uuid: UUID, author: UUID, label: String, tags: Seq[String], reminder: Reminder)

case class Reminder(when: Option[ZonedDateTime], repeat: Boolean, note: Option[String])