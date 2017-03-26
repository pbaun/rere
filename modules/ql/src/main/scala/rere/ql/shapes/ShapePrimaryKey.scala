package rere.ql.shapes

sealed trait ShapePrimaryKey[+T]

object PrimaryKey {
  def apply[T]: ShapePrimaryKey[T] = new ShapePrimaryKey[T] {}
}
