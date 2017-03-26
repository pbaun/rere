package rere.example.todo

import java.util.UUID

import rere.ql.shapes.{IdeaTypeHint, PrimaryKey, Shape}

object TaskShape
  extends Shape(Task.apply _, PrimaryKey[UUID])
  with IdeaTypeHint[Task] {

  implicit val uuid = field("id", _.uuid)
  implicit val author = field("author_id", _.author)
  implicit val label = field("label", _.label)
  implicit val tags = field("tags", _.tags)
  implicit val reminder = sub("reminder", _.reminder, ReminderShape)

  def primaryKey = pk(uuid)
  def projection = uuid :-: author :-: label :-: tags :-: reminder :-: SNil
}

object ReminderShape
  extends Shape(Reminder.apply _, PrimaryKey[Nothing])
  with IdeaTypeHint[Reminder] {

  implicit val when = field("when", _.when)
  implicit val repeat = field("repeat", _.repeat)
  implicit val note = field("note", _.note)

  def primaryKey = noPk
  def projection = when :-: repeat :-: note :-: SNil
}
