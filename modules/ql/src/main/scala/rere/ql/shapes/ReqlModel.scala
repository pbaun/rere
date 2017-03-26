package rere.ql.shapes

import rere.ql.types.ReqlObject
import rere.ql.values.ReqlObjectModel

trait ReqlModel[T] extends ReqlObject

object ReqlModel {
  implicit def shapableToModel[T, PK](model: T)(implicit shape: ModelShape[T, PK]): ReqlModel[T] = {
    new ReqlObjectModel[T](ModelShape[T, PK].toReqlObject(model))
  }
}
