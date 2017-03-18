package rere.ql.shapes

import rere.ql.types.ReqlObject
import rere.ql.values.ReqlObjectModel

trait ReqlModel[T] extends ReqlObject

object ReqlModel {
  implicit def shapableToModel[T : ModelShape](model: T): ReqlModel[T] = {
    new ReqlObjectModel[T](ModelShape[T].toReqlObject(model))
  }
}
