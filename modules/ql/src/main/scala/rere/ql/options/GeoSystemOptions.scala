package rere.ql.options

import rere.ql.queries.values

trait GeoSystemOptions {

  sealed trait GeoSystemOptions extends ComposableOptions

  case object DefaultGeoSystem extends GeoSystemOptions with DefaultOption

  case object WGS84 extends GeoSystemOptions with NonDefaultOption {
    def view = "geo_system" -> values.expr("WGS84") :: Nil
  }

  case object UnitSphere extends GeoSystemOptions with NonDefaultOption {
    def view = "geo_system" -> values.expr("unit_sphere") :: Nil
  }

}
