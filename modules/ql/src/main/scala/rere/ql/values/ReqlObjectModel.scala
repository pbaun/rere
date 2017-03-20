package rere.ql.values

import rere.ql.options.Options
import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.shapes.ReqlModel
import rere.ql.types.ReqlObject

class ReqlObjectModel[M](obj: ReqlObject) extends ReqlModel[M] {
  override def command = -1
  override def string = "make_obj"
  override def arguments = Nil
  override def options = Options.empty

  override def recursiveRasterizer: recursive.Rasterizer = obj.recursiveRasterizer
  override def trampolinedRasterizer: trampolined.Rasterizer = obj.trampolinedRasterizer
}
