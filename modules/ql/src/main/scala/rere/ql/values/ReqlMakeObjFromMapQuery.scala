package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.rasterization.{Renderer, recursive, trampolined}
import rere.ql.types.{ReqlDatum, ReqlObject}


class ReqlMakeObjFromMapQuery(reqlObj: Map[String, ReqlDatum]) extends ReqlObject {
  def command = TermType.MAKE_OBJ
  def string = "make_obj"
  def arguments = Nil
  def options = Options.empty

  def isEmpty = reqlObj.isEmpty

  override def getRasterizer(renderer: Renderer): recursive.Rasterizer = {
    new recursive.ObjRasterizer(renderer, reqlObj)
  }

  override def getTrampolinedRasterizer(renderer: Renderer): trampolined.Rasterizer = {
    new trampolined.ObjRasterizer(renderer, reqlObj)
  }
}
