package rere.ql.rasterization

import java.nio.charset.StandardCharsets

import akka.util.ByteString
import rere.ql.types.ReqlExpr

object building {
  implicit class BuildOp(val query: ReqlExpr) extends AnyVal {
    def build(): ByteString = {
      import cats.instances.function._

      val renderer = new ByteStringRenderer(StandardCharsets.UTF_8)
      val rasterizer = query.getTrampolinedRasterizer(renderer)
      rasterizer.rasterize().run.get
    }
  }
}
