package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types.{ReqlLine, ReqlPoint}

class ReqlLineQuery(point1: ReqlPoint, point2: ReqlPoint, otherPoints: ReqlPoint*) extends ReqlLine {
  val command = TermType.LINE
  val string = "line"
  val arguments = point1 :: point2 :: otherPoints.toList
  val options = Options.empty
}
