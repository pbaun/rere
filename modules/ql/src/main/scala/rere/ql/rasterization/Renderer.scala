package rere.ql.rasterization

import java.nio.charset.Charset

import akka.util.ByteString

trait Renderer {
  def ~~(string: String): this.type
  def ~~(byteString: ByteString): this.type
  def get: ByteString
}

class ByteStringRenderer(encoding: Charset) extends Renderer {

  import akka.util._

  val builder = new ByteStringBuilder

  def ~~(str: String): this.type = {
    builder.putBytes(str.getBytes(encoding))
    this
  }

  def ~~(byteString: ByteString): this.type = {
    builder ++= byteString
    this
  }

  def get: ByteString = builder.result()
}
