package rere.ql.queries

import rere.ql.options.all._
import rere.ql.options.{ComposableOptions, Options}
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait GeospatialQueries {

  // circle
  trait CircleQuery extends ReqlGeometry
  //TODO: filled circle - polygon; not filled - line
  //TODO: maybe deprecate constructing from arrays?

  implicit class CircleOpOnR(val r: ReqlR) {
    def circle(
      center: ReqlPoint,
      radius: ReqlFloat,
      numVertices: NumVerticesOptions = DefaultNumVertices,
      geoSystem: GeoSystemOptions = DefaultGeoSystem,
      unit: DistanceUnitOptions = DefaultDistanceUnit,
      fill: CircleFillOptions = DefaultCircleFill
    ): CircleQuery = new CircleQuery {
      val command = TermType.CIRCLE
      val string = "circle"
      val arguments = center :: radius :: Nil
      val options = ComposableOptions.compose(numVertices, geoSystem, unit, fill)
    }

    /*def circle(
      center: ReqlArray,
      radius: ReqlFloat,
      numVertices: NumVerticesOptions = DefaultNumVertices,
      geoSystem: GeoSystemOptions = DefaultGeoSystem,
      unit: DistanceUnitOptions = DefaultDistanceUnit,
      fill: CircleFillOptions = DefaultCircleFill): CircleQuery = new CircleQuery {

      val command = TermType.CIRCLE
      val string = "circle"
      val arguments = center :: radius :: Nil
      val options = ComposableOptions.compose(numVertices, geoSystem, unit, fill)
    }*/
  }

  // distance
  trait DistanceQuery extends ReqlFloat
  //TODO: implement it on r

  implicit class DistanceOpOnPoint(val point: ReqlPoint) {
    def distance(
      geometry: ReqlGeometry,
      geoSystem: GeoSystemOptions = DefaultGeoSystem,
      unit: DistanceUnitOptions = DefaultDistanceUnit
    ): DistanceQuery = new DistanceQuery {
      val command = TermType.DISTANCE
      val string = "distance"
      val arguments = point :: geometry :: Nil
      val options = ComposableOptions.compose(geoSystem, unit)
    }
  }

  implicit class DistanceOpOnLine(val line: ReqlLine) {
    def distance(
      point: ReqlPoint,
      geoSystem: GeoSystemOptions = DefaultGeoSystem,
      unit: DistanceUnitOptions = DefaultDistanceUnit
    ): DistanceQuery = new DistanceQuery {
      val command = TermType.DISTANCE
      val string = "distance"
      val arguments = line :: point :: Nil
      val options = ComposableOptions.compose(geoSystem, unit)
    }
  }

  implicit class DistanceOpOnPolygon(val polygon: ReqlPolygon) {
    def distance(
      point: ReqlPoint,
      geoSystem: GeoSystemOptions = DefaultGeoSystem,
      unit: DistanceUnitOptions = DefaultDistanceUnit
    ): DistanceQuery = new DistanceQuery {
      val command = TermType.DISTANCE
      val string = "distance"
      val arguments = polygon :: point :: Nil
      val options = ComposableOptions.compose(geoSystem, unit)
    }
  }

  // fill
  trait FillQuery extends ReqlPolygon
  //TODO: line should contain 3 or more points

  implicit class FillOpOnLine(val line: ReqlLine) {
    def fill(): FillQuery = new FillQuery {
      val command = TermType.FILL
      val string = "fill"
      val arguments = line :: Nil
      val options = Options.empty
    }
  }

  // geojson
  trait GeoJsonQuery extends ReqlGeometry

  implicit class GeoJsonOpOnR(val r: ReqlR) {
    def geojson(geojson: ReqlObject): GeoJsonQuery = new GeoJsonQuery {
      val command = TermType.GEOJSON
      val string = "geojson"
      val arguments = geojson :: Nil
      val options = Options.empty
    }
  }

  // to_geojson
  trait ToGeoJsonQuery extends ReqlObject

  implicit class ToGeoJsonOpOnGeometry(val geometry: ReqlGeometry) {
    def toGeojson(): ToGeoJsonQuery = new ToGeoJsonQuery {
      val command = TermType.TO_GEOJSON
      val string = "to_geojson"
      val arguments = geometry :: Nil
      val options = Options.empty
    }
  }

  // get_intersecting
  trait GetIntersectingTableQuery[T, PK] extends ReqlSelectionOfStream[T, PK]

  implicit class GetIntersectingOpOnTable[T, PK](val table: ReqlTable[T, PK]) {
    def getIntersecting(
      geometry: ReqlGeometry,
      geoIndex: Index
    ): GetIntersectingTableQuery[T, PK] = new GetIntersectingTableQuery[T, PK] {
      val command = TermType.GET_INTERSECTING
      val string = "get_intersecting"
      val arguments = table :: geometry :: Nil
      val options = ComposableOptions.compose(geoIndex)
    }
  }

  // get_nearest
  trait GetNearestQuery[T] extends ReqlArray[ReqlDistanceResult[T]]

  implicit class GetNearestOpOnTable[T, PK](val table: ReqlTable[T, PK]) {
    def getNearest(
      point: ReqlPoint,
      geoIndex: Index,
      maxResults: MaxResultsOptions = DefaultMaxResults,
      maxDist: MaxDistanceOptions = DefaultMaxDistance,
      unit: DistanceUnitOptions = DefaultDistanceUnit,
      geoSystem: GeoSystemOptions = DefaultGeoSystem
    ): GetNearestQuery[T] = new GetNearestQuery[T] {
      val command = TermType.GET_NEAREST
      val string = "get_nearest"
      val arguments = table :: point :: Nil
      val options = ComposableOptions.compose(geoIndex, maxResults, maxDist, unit, geoSystem)
    }
  }

  // includes
  trait IncludesArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  //TODO: implement on finite and infinite streams
  //TODO: implement on geometry

  implicit class IncludesOpOnArray[T <: ReqlDatum](val array: ReqlArray[T]) {
    def includes(geometry: ReqlGeometry): IncludesArrayQuery[T] = new IncludesArrayQuery[T] {
      val command = TermType.INCLUDES
      val string = "includes"
      val arguments = array :: geometry :: Nil
      val options = Options.empty
    }
  }

  // intersects
  trait IntersectsArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  //TODO: implement on finite and infinite streams
  //TODO: implement on r
  //TODO: implement on geometry

  implicit class IntersectsOpOnArray[T <: ReqlDatum](val array: ReqlArray[T]) {
    def intersects(geometry: ReqlGeometry): IntersectsArrayQuery[T] = new IntersectsArrayQuery[T] {
      val command = TermType.INTERSECTS
      val string = "intersects"
      val arguments = array :: geometry :: Nil
      val options = Options.empty
    }
  }

  // line
  trait LineQuery extends ReqlLine
  //TODO: maybe deprecate constructing from arrays?

  implicit class LineOpOnR(val r: ReqlR) {
    def line(point1: ReqlPoint, point2: ReqlPoint, otherPoints: ReqlPoint*): LineQuery = new LineQuery {
      val command = TermType.LINE
      val string = "line"
      val arguments = point1 :: point2 :: otherPoints.toList
      val options = Options.empty
    }

    def line(point1: ReqlArray[ReqlNumber], point2: ReqlArray[ReqlNumber], otherPoints: ReqlArray[ReqlNumber]*): LineQuery = new LineQuery {
      val command = TermType.LINE
      val string = "line"
      val arguments = point1 :: point2 :: otherPoints.toList
      val options = Options.empty
    }
  }

  // point
  trait PointQuery extends ReqlPoint

  implicit class PointOpOnR(val r: ReqlR) {
    def point(longitude: ReqlNumber, latitude: ReqlNumber): PointQuery = new PointQuery {
      val command = TermType.POINT
      val string = "point"
      val arguments = longitude :: latitude :: Nil
      val options = Options.empty
    }
  }

  // polygon
  trait PolygonQuery extends ReqlPolygon
  //TODO: maybe deprecate constructing from arrays?

  implicit class PolygonOpOnR(val r: ReqlR) {
    def polygon(point1: ReqlPoint,
                point2: ReqlPoint,
                point3: ReqlPoint,
                otherPoints: ReqlPoint*): PolygonQuery = new PolygonQuery {
      val command = TermType.POLYGON
      val string = "polygon"
      val arguments = point1 :: point2 :: point3 :: otherPoints.toList
      val options = Options.empty
    }

    def polygon(point1: ReqlArray[ReqlNumber],
                point2: ReqlArray[ReqlNumber],
                point3: ReqlArray[ReqlNumber],
                otherPoints: ReqlArray[ReqlNumber]*): PolygonQuery = new PolygonQuery {
      val command = TermType.POLYGON
      val string = "polygon"
      val arguments = point1 :: point2 :: point3 :: otherPoints.toList
      val options = Options.empty
    }
  }

  // polygon_sub
  trait PolygonSubQuery extends ReqlPolygon

  implicit class PolygonSubOpOnPolygon(val polygon: ReqlPolygon) {
    def polygonSub(subtractedPolygon: ReqlPolygon): PolygonSubQuery = new PolygonSubQuery {
      val command = TermType.POLYGON_SUB
      val string = "polygon_sub"
      val arguments = polygon :: subtractedPolygon :: Nil
      val options = Options.empty
    }
  }

}
