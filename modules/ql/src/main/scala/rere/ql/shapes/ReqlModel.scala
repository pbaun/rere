package rere.ql.shapes

import io.circe.JsonObject
import rere.ql.types.{ReqlObject, ReqlShapable}
import rere.ql.values.ReqlObjectModel

trait ReqlModel[T, PK] extends ReqlShapable[T, PK] with ReqlObject

object ReqlModel {
  implicit def shapableToModel[T, PK](model: T)(
    implicit shape: ModelShape[T, PK]
  ): ReqlModel[T, PK] = {
    new ReqlObjectModel[T, PK](model, shape)
  }
  implicit def jsonToModel[PK](model: JsonObject)(
    implicit shape: ModelShape[JsonObject, PK]
  ): ReqlModel[JsonObject, PK] = {
    new ReqlObjectModel[JsonObject, PK](model, shape)
  }
}
