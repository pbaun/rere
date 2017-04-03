package rere.sasl.scram.rendering

import rere.sasl.util.StringRenderer

object SCRAMRenderer {
  def renderToString[T : SCRAMRendering](obj: T): String = {
    val renderer = new StringRenderer
    SCRAMRendering[T].render(renderer, obj).get
  }
}
