package rere.sasl.util

trait Renderer {
  def ~~(string: String): this.type
  def ~~[T : Rendering](renderable: T): this.type
  def ~~[T : Rendering](opt: Option[T]): this.type
}

object Renderer {
  def renderToString[T : Rendering](obj: T): String = {
    val renderer = new StringRenderer
    Rendering[T].render(renderer, obj).get
  }
}

trait Rendering[T] {
  def render[R <: Renderer](r: R, obj: T): R
}

object Rendering {
  def apply[T](implicit rendering: Rendering[T]): Rendering[T] = rendering
}

class StringRenderer extends Renderer {
  private val builder = new java.lang.StringBuilder(128)

  override def ~~(str: String): this.type = {
    builder.append(str)
    this
  }

  override def ~~[T : Rendering](renderable: T): this.type = {
    Rendering[T].render(this, renderable)
  }

  override def ~~[T : Rendering](opt: Option[T]): this.type = {
    opt match {
      case Some(renderable) => Rendering[T].render(this, renderable)
      case None => this
    }
  }

  def get: String = builder.toString
}
