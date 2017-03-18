package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.rasterization.{Renderer, recursive, trampolined}
import rere.ql.types.{ReqlDatum, ReqlObject}

class ReqlMakeObjFromPairsListQuery(pairs: List[(String, ReqlDatum)]) extends ReqlObject {
  def command = TermType.MAKE_OBJ
  def string = "make_obj"
  def arguments = Nil
  def options = Options.empty

  def isEmpty = pairs.isEmpty

  override def getRasterizer(renderer: Renderer): recursive.Rasterizer = {
    new recursive.ListOfPairsRasterizer(renderer, pairs)
  }

  override def getTrampolinedRasterizer(renderer: Renderer): trampolined.Rasterizer = {
    new trampolined.ListOfPairsRasterizer(renderer, pairs)
  }
}
