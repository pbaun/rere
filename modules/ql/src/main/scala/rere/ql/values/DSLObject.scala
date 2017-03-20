package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.queries.{DSLKeyValuePair, DSLKeyValuePairList}
import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.types.ReqlObject

trait DSLObject extends ReqlObject {
  def ~ (anotherPair: DSLKeyValuePair): DSLKeyValuePairList

  protected def pairs: List[DSLKeyValuePair]

  def command = TermType.MAKE_OBJ
  def string = "make_obj"
  def arguments = Nil
  def options = Options.empty

  def isEmpty = pairs.isEmpty

  override def recursiveRasterizer: recursive.Rasterizer = {
    new recursive.ListOfDSLPairsRasterizer(pairs)
  }

  override def trampolinedRasterizer: trampolined.Rasterizer = {
    new trampolined.ListOfDSLPairsRasterizer(pairs)
  }
}
