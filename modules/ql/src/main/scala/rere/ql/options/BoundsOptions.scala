package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.ReqlValue

trait BoundsOptions {

  sealed trait BoundType {
    def rqlValue: ReqlValue
  }
  case object OpenBound extends BoundType {
    val rqlValue = values.expr("open")
  }
  case object ClosedBound extends BoundType {
    val rqlValue = values.expr("closed")
  }
  case object DefaultBound extends BoundType {
    val rqlValue = values.expr(null)
  }

  sealed trait BoundsOptions extends ComposableOptions

  case object DefaultBounds extends BoundsOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class Bounds(leftBound: BoundType, rightBound: BoundType) extends BoundsOptions {
    def isEmpty = innerQuery.isEmpty
    def view: List[(String, ReqlValue)] = {
      val leftView = leftBound match {
        case DefaultBound => Nil
        case left => "left_bound" -> left.rqlValue :: Nil
      }

      val rightView = rightBound match {
        case DefaultBound => Nil
        case right => "right_bound" -> right.rqlValue :: Nil
      }

      leftView ::: rightView
    }
    def innerQuery = query
  }

}