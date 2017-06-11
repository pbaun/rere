package rere.ql.options

import rere.ql.queries.values
import rere.ql.types.{ReqlObject, ReqlValue}

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

  case object DefaultBounds extends BoundsOptions with DefaultOption

  case class Bounds(leftBound: BoundType, rightBound: BoundType) extends BoundsOptions {
    override def isEmpty: Boolean = (leftBound == DefaultBound) && (rightBound == DefaultBound)
    override def view: ComposableOptions.View = {
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
    override def expr: ReqlObject = exprFromView
  }

}