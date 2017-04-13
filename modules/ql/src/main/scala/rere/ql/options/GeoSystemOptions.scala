package rere.ql.options

import rere.ql.queries.values

trait GeoSystemOptions {

  sealed trait GeoSystemOptions extends ComposableOptions

  case object DefaultGeoSystem extends GeoSystemOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case object WGS84 extends GeoSystemOptions {
    def isEmpty = false
    def view = "geo_system" -> values.expr("WGS84") :: Nil
    val expr = exprFromView
  }

  case object UnitSphere extends GeoSystemOptions {
    def isEmpty = false
    def view = "geo_system" -> values.expr("unit_sphere") :: Nil
    val expr = exprFromView
  }

}
