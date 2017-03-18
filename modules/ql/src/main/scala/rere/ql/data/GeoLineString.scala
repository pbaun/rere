package rere.ql.data

import rere.ql.types.{ReqlLine, ReqlPoint}
import rere.ql.values.ReqlLineQuery

case class GeoLineString(point1: GeoPoint, point2: GeoPoint, otherPoints: GeoPoint*)

object GeoLineString {
  implicit def toReqlLine(implicit line: GeoLineString): ReqlLine = {
    new ReqlLineQuery(line.point1, line.point2, line.otherPoints.map(x => x: ReqlPoint): _*)
  }
}
