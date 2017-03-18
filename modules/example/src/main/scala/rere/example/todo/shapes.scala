package rere.example.todo

import rere.ql.shapes.{IdeaTypeHint, Shape}

object TaskShape
  extends Shape(Task.apply _)
  with IdeaTypeHint[Task] {

  implicit val uuid = field("id", _.uuid)
  implicit val author = field("author_id", _.author)
  implicit val label = field("label", _.label)
  implicit val tags = field("tags", _.tags)
  implicit val reminder = sub("reminder", _.reminder, ReminderShape)

  def projection: Projection = uuid :-: author :-: label :-: tags :-: reminder :-: SNil
}

object ReminderShape
  extends Shape(Reminder.apply _)
  with IdeaTypeHint[Reminder] {

  implicit val when = field("when", _.when)
  implicit val repeat = field("repeat", _.repeat)
  implicit val note = field("note", _.note)

  def projection: Projection = when :-: repeat :-: note :-: SNil
}
