package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.queries.{DSLKeyValuePair, DSLKeyValuePairList}
import rere.ql.rasterization.{Renderer, recursive, trampolined}
import rere.ql.types.ReqlObject

trait DSLObject extends ReqlObject {
  def ~ (anotherPair: DSLKeyValuePair): DSLKeyValuePairList

  protected def pairs: List[DSLKeyValuePair]

  def command = TermType.MAKE_OBJ
  def string = "make_obj"
  def arguments = Nil
  def options = Options.empty

  def isEmpty = pairs.isEmpty

  override def getRasterizer(renderer: Renderer): recursive.Rasterizer = {
    new recursive.ListOfDSLPairsRasterizer(renderer, pairs)
  }

  override def getTrampolinedRasterizer(renderer: Renderer): trampolined.Rasterizer = {
    new trampolined.ListOfDSLPairsRasterizer(renderer, pairs)
  }
}
