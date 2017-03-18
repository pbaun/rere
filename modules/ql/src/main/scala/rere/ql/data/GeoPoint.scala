package rere.ql.data

import rere.ql.queries.values
import rere.ql.types.ReqlPoint
import rere.ql.values.ReqlPointQuery

case class GeoPoint(longitude: Double, latitude: Double)

object GeoPoint {
  implicit def toReqlPoint(point: GeoPoint): ReqlPoint = {
    new ReqlPointQuery(values.expr(point.longitude), values.expr(point.latitude))
  }
}
