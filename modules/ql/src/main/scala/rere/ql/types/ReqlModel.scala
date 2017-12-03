package rere.ql.types

import io.circe.JsonObject
import rere.ql.shapes.ModelShape
import rere.ql.values.ReqlObjectModel

trait ReqlModel[T, PK <: PrimaryKey] extends ReqlShapable[T, PK] with ReqlObject

object ReqlModel {
  implicit def shapableToModel[T, PK <: PrimaryKey](model: T)(
    implicit shape: ModelShape[T, PK]
  ): ReqlModel[T, PK] = {
    new ReqlObjectModel[T, PK](model, shape)
  }
  implicit def jsonToModel[PK <: PrimaryKey](model: JsonObject)(
    implicit shape: ModelShape[JsonObject, PK]
  ): ReqlModel[JsonObject, PK] = {
    new ReqlObjectModel[JsonObject, PK](model, shape)
  }
}
