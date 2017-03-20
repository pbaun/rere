package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.types.{ReqlDatum, ReqlObject}


class ReqlMakeObjFromMapQuery(reqlObj: Map[String, ReqlDatum]) extends ReqlObject {
  def command = TermType.MAKE_OBJ
  def string = "make_obj"
  def arguments = Nil
  def options = Options.empty

  def isEmpty = reqlObj.isEmpty

  override def recursiveRasterizer: recursive.Rasterizer = {
    new recursive.ObjRasterizer(reqlObj)
  }

  override def trampolinedRasterizer: trampolined.Rasterizer = {
    new trampolined.ObjRasterizer(reqlObj)
  }
}
