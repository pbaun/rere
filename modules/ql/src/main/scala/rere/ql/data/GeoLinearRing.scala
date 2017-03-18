package rere.ql.data

import rere.ql.types.{ReqlPoint, ReqlPolygon}
import rere.ql.values.ReqlPolygonQuery

case class GeoLinearRing(point1: GeoPoint, point2: GeoPoint, point3: GeoPoint, otherPoints: GeoPoint*)

object GeoLinearRing {
  implicit def toReqlPolygon(implicit ring: GeoLinearRing): ReqlPolygon = {
    new ReqlPolygonQuery(ring.point1, ring.point2, ring.point3, ring.otherPoints.map(x => x: ReqlPoint): _*)
  }
}
