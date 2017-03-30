package rere.ql.values

import rere.ql.options.Options
import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.shapes.{ModelShape, ReqlModel}

class ReqlObjectModel[M, PK](model: M, val shape: ModelShape[M, PK]) extends ReqlModel[M, PK] {
  private val obj = shape.toReqlObject(model)

  override def command = -1
  override def string = "make_obj"
  override def arguments = Nil
  override def options = Options.empty

  override def recursiveRasterizer: recursive.Rasterizer = obj.recursiveRasterizer
  override def trampolinedRasterizer: trampolined.Rasterizer = obj.trampolinedRasterizer
}
