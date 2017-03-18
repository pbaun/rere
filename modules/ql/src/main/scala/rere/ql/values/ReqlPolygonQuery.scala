package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types.{ReqlPoint, ReqlPolygon}

class ReqlPolygonQuery(point1: ReqlPoint, point2: ReqlPoint, point3: ReqlPoint, otherPoints: ReqlPoint*) extends ReqlPolygon {
  val command = TermType.POLYGON
  val string = "polygon"
  val arguments = point1 :: point2 :: point3 :: otherPoints.toList
  val options = Options.empty
}
