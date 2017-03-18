package rere.ql.rasterization

import akka.util.ByteString
import io.circe.Json
import rere.ql.queries.DSLKeyValuePair
import rere.ql.types.ReqlExpr

object recursive {

  trait Rasterizer {
    def rasterize(): Renderer
  }

  class QueryRasterizer(renderer: Renderer, query: ReqlExpr) extends Rasterizer {

    def rasterize(): Renderer = {
      renderer ~~ "[" ~~ query.command.toString ~~ ","

      renderer ~~ "["
      var first = true
      query.arguments foreach { arg =>
        if (first) {
          arg.getRasterizer(renderer).rasterize()
          first = false
        } else {
          renderer ~~ ","
          arg.getRasterizer(renderer).rasterize()
        }
      }
      renderer ~~ "]"

      if (!query.options.isEmpty) {
        renderer ~~ ","
        query.options.getRasterizer(renderer).rasterize()
      }

      renderer ~~ "]"
    }
  }

  class PrimitiveRasterizer(renderer: Renderer, repr: String) extends Rasterizer {
    def rasterize(): Renderer = {
      renderer ~~ repr
    }
  }

  class ObjRasterizer(renderer: Renderer, recursiveMap: Map[String, ReqlExpr]) extends Rasterizer {
    private def encloseJsonString(value: String): String = Json.fromString(value).noSpaces

    def rasterize(): Renderer = {
      renderer ~~ "{"

      var first = true
      recursiveMap foreach {
        case (key, value) =>
          if (first) {
            first = false
          } else {
            renderer ~~ ","
          }
          renderer ~~ encloseJsonString(key) ~~ ":"
          value.getRasterizer(renderer).rasterize()
      }

      renderer ~~ "}"
    }
  }

  class ListOfPairsRasterizer(renderer: Renderer, pairs: List[(String, ReqlExpr)]) extends Rasterizer {
    private def encloseJsonString(value: String): String = Json.fromString(value).noSpaces

    def rasterize(): Renderer = {
      if (pairs.isEmpty) {
        renderer ~~ "{}"
      } else {
        renderer ~~ "{"

        var first = true
        pairs foreach {
          case (key, value) =>
            if (first) {
              first = false
            } else {
              renderer ~~ ","
            }
            renderer ~~ encloseJsonString(key) ~~ ":"
            value.getRasterizer(renderer).rasterize()
        }

        renderer ~~ "}"
      }
    }
  }

  class ListOfDSLPairsRasterizer(renderer: Renderer, pairs: List[DSLKeyValuePair]) extends Rasterizer {
    def rasterize(): Renderer = {
      if (pairs.isEmpty) {
        renderer ~~ "{}"
      } else {
        renderer ~~ "{"

        var first = true
        pairs foreach {
          pair: DSLKeyValuePair =>
            if (first) {
              first = false
            } else {
              renderer ~~ ","
            }
            pair.key.getRasterizer(renderer)
            renderer ~~ ":"
            pair.datum.getRasterizer(renderer).rasterize()
        }

        renderer ~~ "}"
      }
    }
  }

  private val base64Encoder = java.util.Base64.getEncoder

  class BinaryRasterizer(renderer: Renderer, binary: ByteString) extends Rasterizer {
    def rasterize(): Renderer = {
      renderer ~~ """{"$reql_type$":"BINARY","data":""""
      renderer ~~ ByteString(base64Encoder.encode(binary.asByteBuffer))
      renderer ~~ """"}"""
    }
  }
}
