package rere.ql.values

import akka.util.ByteString
import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.rasterization.{Renderer, recursive, trampolined}
import rere.ql.types.ReqlBinary

class ReqlBinaryQuery(binary: ByteString) extends ReqlBinary {
  def command = TermType.BINARY
  def string = "binary"
  def arguments = Nil
  def options = Options.empty

  override def getRasterizer(renderer: Renderer): recursive.Rasterizer = {
    new recursive.BinaryRasterizer(renderer, binary)
  }

  override def getTrampolinedRasterizer(renderer: Renderer): trampolined.Rasterizer = {
    new trampolined.BinaryRasterizer(renderer, binary)
  }
}
