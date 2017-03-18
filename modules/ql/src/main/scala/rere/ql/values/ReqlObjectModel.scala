package rere.ql.values

import rere.ql.options.Options
import rere.ql.rasterization.{Renderer, recursive, trampolined}
import rere.ql.shapes.ReqlModel
import rere.ql.types.ReqlObject

class ReqlObjectModel[M](obj: ReqlObject) extends ReqlModel[M] {
  override def command = -1
  override def string = "make_obj"
  override def arguments = Nil
  override def options = Options.empty

  override def getRasterizer(renderer: Renderer): recursive.Rasterizer = {
    obj.getRasterizer(renderer)
  }

  override def getTrampolinedRasterizer(renderer: Renderer): trampolined.Rasterizer = {
    obj.getTrampolinedRasterizer(renderer)
  }
}
