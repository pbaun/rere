package rere.ql.rasterization

import akka.util.ByteString
import cats.free.Trampoline
import cats.free.Trampoline._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe.Json
import rere.ql.queries.DSLKeyValuePair
import rere.ql.types.ReqlExpr

object trampolined {

  trait Rasterizer {
    def rasterize(renderer: Renderer): Trampoline[Renderer]
  }

  class QueryRasterizer(query: ReqlExpr) extends Rasterizer {
    def rasterize(renderer: Renderer): Trampoline[Renderer] = {

      def rasterizeArgs(r: Renderer): Trampoline[Renderer] = {
        query.arguments.zipWithIndex.traverse[Trampoline, Renderer] {
          case (arg, index) =>
            for {
              r <- if (index == 0) done(r) else delay(r ~~ ",")
              r <- arg.trampolinedRasterizer.rasterize(r)
            } yield r
        } map { renderers =>
          if (renderers.nonEmpty) renderers.last else r
        }
      }

      def rasterizeOptions(r: Renderer): Trampoline[Renderer] = {
        query.options.trampolinedRasterizer.rasterize(r)
      }

      for {
        r <- done(renderer ~~ "[" ~~ query.command.toString ~~ ",[")
        r <- rasterizeArgs(r)
        r <- done(r ~~ "]")
        r <- suspend {
          if (!query.options.isEmpty) {
            for {
              r <- done(r ~~ ",")
              r <- rasterizeOptions(r)
            } yield r
          } else {
            done(r)
          }
        }
        r <- done(r ~~ "]")
      } yield r
    }
  }

  class PrimitiveRasterizer(representation: String) extends Rasterizer {
    def rasterize(renderer: Renderer): Trampoline[Renderer] = {
      done(renderer ~~ representation)
    }
  }

  class ObjRasterizer(obj: Map[String, ReqlExpr]) extends Rasterizer {

    private def encloseJsonString(value: String): String = Json.fromString(value).noSpaces

    def rasterize(renderer: Renderer): Trampoline[Renderer] = {
      for {
        r <- done(renderer ~~ "{")
        r <- {
          obj.toList.zipWithIndex.traverse[Trampoline, Renderer] {
            case ((key, value), index) =>
              for {
                r <- if (index == 0) done(r) else delay(r ~~ ",")
                r <- done(r ~~ encloseJsonString(key) ~~ ":")
                r <- value.trampolinedRasterizer.rasterize(r)
              } yield r
          } map { renderers =>
            if (renderers.nonEmpty) renderers.last else r
          }
        }
        r <- done(r ~~ "}")
      } yield r
    }
  }

  class ListOfPairsRasterizer(pairs: List[(String, ReqlExpr)]) extends Rasterizer {

    private def encloseJsonString(value: String): String = Json.fromString(value).noSpaces

    def rasterize(renderer: Renderer): Trampoline[Renderer] = {
      if (pairs.isEmpty) {
        done(renderer ~~ "{}")
      } else {
        for {
          r <- done(renderer ~~ "{")
          r <- {
            pairs.zipWithIndex.traverse[Trampoline, Renderer] {
              case ((key, value), index) =>
                for {
                  r <- if (index == 0) done(r) else delay(r ~~ ",")
                  r <- done(r ~~ encloseJsonString(key) ~~ ":")
                  r <- value.trampolinedRasterizer.rasterize(r)
                } yield r
            } map { renderers =>
              if (renderers.nonEmpty) renderers.last else r
            }
          }
          r <- done(r ~~ "}")
        } yield r
      }
    }
  }

  class ListOfDSLPairsRasterizer(pairs: List[DSLKeyValuePair]) extends Rasterizer {

    def rasterize(renderer: Renderer): Trampoline[Renderer] = {
      if (pairs.isEmpty) {
        done(renderer ~~ "{}")
      } else {
        for {
          r <- done(renderer ~~ "{")
          r <- {
            pairs.zipWithIndex.traverse[Trampoline, Renderer] {
              case (pair, index) =>
                for {
                  r <- if (index == 0) done(r) else delay(r ~~ ",")
                  r <- pair.key.trampolinedRasterizer.rasterize(r)
                  r <- done(r ~~ ":")
                  r <- pair.datum.trampolinedRasterizer.rasterize(r)
                } yield r
            } map { renderers =>
              if (renderers.nonEmpty) renderers.last else r
            }
          }
          r <- done(r ~~ "}")
        } yield r
      }
    }
  }

  private val base64Encoder = java.util.Base64.getEncoder

  class BinaryRasterizer(binary: ByteString) extends Rasterizer {
    def rasterize(renderer: Renderer): Trampoline[Renderer] = {
      for {
        r <- done(renderer ~~ """{"$reql_type$":"BINARY","data":"""")
        r <- done(r ~~ ByteString(base64Encoder.encode(binary.asByteBuffer)))
        r <- done(r ~~ """"}""")
      } yield r
    }
  }
}
