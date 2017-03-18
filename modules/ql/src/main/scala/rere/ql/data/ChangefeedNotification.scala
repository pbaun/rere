package rere.ql.data

case class ChangefeedNotification[T](oldVal: Option[T], newVal: Option[T])
