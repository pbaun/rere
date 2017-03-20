package rere.ql.values

import akka.util.ByteString
import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.types.ReqlBinary

class ReqlBinaryQuery(binary: ByteString) extends ReqlBinary {
  def command = TermType.BINARY
  def string = "binary"
  def arguments = Nil
  def options = Options.empty

  override def recursiveRasterizer: recursive.Rasterizer = {
    new recursive.BinaryRasterizer(binary)
  }

  override def trampolinedRasterizer: trampolined.Rasterizer = {
    new trampolined.BinaryRasterizer(binary)
  }
}
