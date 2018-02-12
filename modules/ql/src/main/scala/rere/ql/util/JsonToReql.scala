package rere.ql.util

import cats.free.Trampoline
import cats.instances.function._
import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.Json
import rere.ql.ql2.Term.TermType

/**
  * Reql needs transformation of arrays to make_array call
  */
object JsonToReql {

  def transform(json: Json): Json = transformArrays(json).run

  private def transformArrays(json: Json): Trampoline[Json] = {
    json.arrayOrObject(
      Trampoline.done(json),
      array => {
        val traversed = array.traverse[Trampoline, Json](value => Trampoline.defer(transformArrays(value)))
        traversed map { values =>
          Json.fromValues(transformArraysToReql(values))
        }
      },
      obj => {
        val traversed = obj.traverse[Trampoline](value => Trampoline.defer(transformArrays(value)))
        traversed map Json.fromJsonObject
      }
    )
  }

  private def transformArraysToReql(jsonArray: Vector[Json]): Vector[Json] = {
    Vector(Json.fromInt(TermType.MAKE_ARRAY), Json.fromValues(jsonArray))
  }
}
