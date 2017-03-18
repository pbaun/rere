package rere.ql.data

case class GeoPolygon(exteriorRing: GeoLinearRing, interiorRings: GeoLinearRing*)

//TODO: use polygonSub?
/*object GeoPolygon {
  implicit def toReqlPolygon(implicit polygon: GeoPolygon): ReqlPolygon = {
    new ReqlPolygonQuery(polygon.point1, polygon.point2, polygon.point3, polygon.otherPoints.map(x => x: ReqlPoint): _*)
  }
}*/
