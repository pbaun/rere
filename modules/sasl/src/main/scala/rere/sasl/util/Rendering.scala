package rere.sasl.util

import java.nio.charset.{Charset, StandardCharsets}

import akka.util.ByteString

trait Renderer {
  def ~~(bytes: Array[Byte]): this.type
  def ~~(string: String): this.type
  def ~~[T](renderable: T)(implicit rendering: Rendering[T]): this.type
  def ~~[T](opt: Option[T])(implicit rendering: Rendering[T]): this.type
}

object Renderer {
  def render[T, R <: Renderer](r: R, obj: T)(implicit rendering: Rendering[T]): R = {
    rendering.render(r, obj)
  }

  def renderToByteString[T](obj: T)(implicit rendering: Rendering[T]): ByteString = {
    val renderer = new ByteStringRenderer(StandardCharsets.UTF_8)
    rendering.render(renderer, obj).get
  }

  def renderToString[T](obj: T)(implicit rendering: Rendering[T]): String = {
    val renderer = new ByteStringRenderer(StandardCharsets.UTF_8)
    rendering.render(renderer, obj).get.utf8String
  }
}

trait Rendering[T] {
  def render[R <: Renderer](r: R, obj: T): R
}

class ByteStringRenderer(encoding: Charset) extends Renderer {
  import akka.util._

  val builder = new ByteStringBuilder

  def ~~(bytes: Array[Byte]): this.type = {
    builder.putBytes(bytes)
    this
  }

  def ~~(str: String): this.type = {
    builder.putBytes(str.getBytes(encoding))
    this
  }

  def ~~[T](renderable: T)(implicit rendering: Rendering[T]): this.type = {
    rendering.render(this, renderable)
  }

  def ~~[T](opt: Option[T])(implicit rendering: Rendering[T]): this.type = {
    opt match {
      case Some(renderable) => rendering.render(this, renderable)
      case None => this
    }
  }

  def get: ByteString = builder.result()
}

