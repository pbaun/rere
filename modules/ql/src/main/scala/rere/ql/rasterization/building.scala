package rere.ql.rasterization

import java.nio.charset.StandardCharsets

import akka.util.ByteString
import rere.ql.types.ReqlExpr

object building {
  implicit class BuildOp(val query: ReqlExpr) extends AnyVal {
    def build(): ByteString = {
      import cats.instances.function._

      val renderer = new ByteStringRenderer(StandardCharsets.UTF_8)
      query.trampolinedRasterizer.rasterize(renderer).run.get
    }

    def buildRecursive(): ByteString = {
      val renderer = new ByteStringRenderer(StandardCharsets.UTF_8)
      query.recursiveRasterizer.rasterize(renderer).get
    }
  }
}
