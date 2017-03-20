package rere.ql.types

import rere.ql.options.Options
import rere.ql.rasterization.{recursive, trampolined}

/**
  *
  * useful links:
  *  - https://github.com/rethinkdb/rethinkdb/blob/next/admin/static/js/reql_docs.js
  *  - https://github.com/rethinkdb/rethinkdb/blob/2993603fa743e0c68b2701b737321afedcb05fd3/src/rdb_protocol/terms/type_manip.cc
  */
trait ReqlExpr {
  def command: Int                 // protocol term type
  def string: String               // term string representation - only for debug
  def arguments: List[ReqlExpr]    // arguments
  def options: Options             // options

  def recursiveRasterizer: recursive.Rasterizer = {
    new recursive.QueryRasterizer(this)
  }

  def trampolinedRasterizer: trampolined.Rasterizer = {
    new trampolined.QueryRasterizer(this)
  }
}

  // Something that can be appended to array and checked by .contains
  trait ReqlDatum extends ReqlExpr

    trait ReqlValue extends ReqlDatum
      trait ReqlNull extends ReqlValue
      trait ReqlBoolean extends ReqlValue
      trait ReqlNumber extends ReqlValue
        trait ReqlInteger extends ReqlNumber
        trait ReqlFloat extends ReqlNumber
      trait ReqlString extends ReqlValue
      trait ReqlArray[+T <: ReqlDatum] extends ReqlValue with ReqlFiniteArrayLike[T]
      trait ReqlObject extends ReqlValue
      trait ReqlJson extends ReqlValue
        trait ReqlJsonObject extends ReqlJson with ReqlObject

    trait ReqlPseudo extends ReqlDatum
      trait ReqlBinary extends ReqlPseudo
      trait ReqlTime extends ReqlPseudo
      trait ReqlGeometry extends ReqlPseudo
        trait ReqlPoint extends ReqlGeometry
        trait ReqlLine extends ReqlGeometry
        trait ReqlPolygon extends ReqlGeometry

  trait ReqlSpecific extends ReqlExpr
    trait ReqlR extends ReqlSpecific
    trait ReqlError extends ReqlSpecific
    trait ReqlJS extends ReqlSpecific
    trait ReqlDatabase extends ReqlSpecific

    /**
      * Sequences traits
      */
    // for union - all sequences (and infinite stream too)
    trait ReqlSequence[+T <: ReqlDatum] extends ReqlSpecific

      /**
        * 1-st classification: finite | infinite
        */
      trait ReqlFiniteSequence[+T <: ReqlDatum] extends ReqlSequence[T]        // all finite sequences - for concatMap and innerJoin
      trait ReqlInfiniteSequence[+T <: ReqlDatum] extends ReqlSequence[T]      // all infinite sequences

      /**
        * 2-nd classification: array | stream
        */
      trait ReqlArrayLike[+T <: ReqlDatum] extends ReqlSequence[T]             // for union - [array | selectionOfArray]
      trait ReqlStreamLike[+T <: ReqlDatum] extends ReqlSequence[T]            // table, slice and streams

      /**
        * products. all sequences extends only one of this trait
        */
      // for union - [arrayLike] (btw all arrays are finite)
      trait ReqlFiniteArrayLike[+T <: ReqlDatum] extends ReqlFiniteSequence[T] with ReqlArrayLike[T]

      // for union - [all - arrayLike - infiniteStream]
      trait ReqlFiniteStreamLike[+T <: ReqlDatum] extends ReqlFiniteSequence[T] with ReqlStreamLike[T]

      // for union - [infiniteStream]
      trait ReqlInfiniteStreamLike[+T <: ReqlDatum] extends ReqlInfiniteSequence[T] with ReqlStreamLike[T]

      /**
        * Implementations
        */
      trait ReqlTable[+T <: ReqlObject] extends ReqlFiniteStreamLike[T]

      trait ReqlTableSlice[+T <: ReqlObject] extends ReqlFiniteStreamLike[T]
      //TODO: table_slice not always behave same way - if it was created in operation with index it can behave differently (.between + .distinct)
      //TODO: maybe between without index should work like selection of array???
      //r.table("tv_shows").between(1,7,{index: "code"}).typeOf() -> "TABLE_SLICE"
      //r.table("tv_shows").between(1,7,{index: "code"}).distinct().typeOf() -> "ARRAY"
      //r.table("tv_shows").between(1,7,{index: "id"}).distinct().typeOf() -> "STREAM"
      //r.table("tv_shows").between(1,7).typeOf() -> "TABLE_SLICE"
      //r.table("tv_shows").between(1,7).distinct().typeOf() -> "STREAM"

      //TODO: selection of array can be used in .union as array (in concatMap it also work like array)
      //TODO: after orderBy will be returned array that can be used for modification of state
      // SELECTION<ARRAY>
      trait ReqlSelectionOfArray[+T <: ReqlObject] extends ReqlFiniteArrayLike[T]

      //TODO: selection of stream can be used in .union as stream
      // SELECTION<STREAM>
      trait ReqlSelectionOfStream[+T <: ReqlObject] extends ReqlFiniteStreamLike[T]

      //infinite - .changes() - can't call .count() on it
      trait ReqlInfiniteStream[+T <: ReqlDatum] extends ReqlInfiniteStreamLike[T]

      //finite - can call .count() on it
      trait ReqlFiniteStream[+T <: ReqlDatum] extends ReqlFiniteStreamLike[T]

      //TODO: what kind of sequence grouped stream and data are?
      trait ReqlGroupedStream[Key, Reduction] extends ReqlSpecific
      trait ReqlGroupedData extends ReqlSpecific

    // SELECTION<OBJECT>
    trait ReqlSelectionOfObject[+T <: ReqlObject] extends ReqlSpecific with ReqlDatum

    trait ReqlFunction extends ReqlSpecific

    trait ReqlLiteral extends ReqlSpecific with ReqlValue

    trait ReqlArgs extends ReqlSpecific

    trait ReqlMinval extends ReqlSpecific
    trait ReqlMaxval extends ReqlSpecific

    //.http returns different types
    trait ReqlHttpResult extends ReqlSpecific


trait ReqlFilterPredicate[T] extends ReqlExpr

trait ReqlOrdering extends ReqlExpr
  trait ReqlNameOrdering extends ReqlOrdering
  trait ReqlLambdaOrdering extends ReqlOrdering
