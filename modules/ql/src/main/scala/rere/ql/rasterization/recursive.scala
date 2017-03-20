package rere.ql.rasterization

import akka.util.ByteString
import io.circe.Json
import rere.ql.queries.DSLKeyValuePair
import rere.ql.types.ReqlExpr

object recursive {

  trait Rasterizer {
    def rasterize(renderer: Renderer): Renderer
  }

  class QueryRasterizer(query: ReqlExpr) extends Rasterizer {

    def rasterize(renderer: Renderer): Renderer = {
      renderer ~~ "[" ~~ query.command.toString ~~ ","

      renderer ~~ "["
      var isFirst = true
      query.arguments foreach { arg =>
        if (isFirst) {
          arg.recursiveRasterizer.rasterize(renderer)
          isFirst = false
        } else {
          renderer ~~ ","
          arg.recursiveRasterizer.rasterize(renderer)
        }
      }
      renderer ~~ "]"

      if (!query.options.isEmpty) {
        renderer ~~ ","
        query.options.recursiveRasterizer.rasterize(renderer)
      }

      renderer ~~ "]"
    }
  }

  class PrimitiveRasterizer(repr: String) extends Rasterizer {
    def rasterize(renderer: Renderer): Renderer = {
      renderer ~~ repr
    }
  }

  class ObjRasterizer(recursiveMap: Map[String, ReqlExpr]) extends Rasterizer {
    private def encloseJsonString(value: String): String = Json.fromString(value).noSpaces

    def rasterize(renderer: Renderer): Renderer = {
      renderer ~~ "{"

      var isFirst = true
      recursiveMap foreach {
        case (key, value) =>
          if (isFirst) {
            isFirst = false
          } else {
            renderer ~~ ","
          }
          renderer ~~ encloseJsonString(key) ~~ ":"
          value.recursiveRasterizer.rasterize(renderer)
      }

      renderer ~~ "}"
    }
  }

  class ListOfPairsRasterizer(pairs: List[(String, ReqlExpr)]) extends Rasterizer {
    private def encloseJsonString(value: String): String = Json.fromString(value).noSpaces

    def rasterize(renderer: Renderer): Renderer = {
      if (pairs.isEmpty) {
        renderer ~~ "{}"
      } else {
        renderer ~~ "{"

        var isFirst = true
        pairs foreach {
          case (key, value) =>
            if (isFirst) {
              isFirst = false
            } else {
              renderer ~~ ","
            }
            renderer ~~ encloseJsonString(key) ~~ ":"
            value.recursiveRasterizer.rasterize(renderer)
        }

        renderer ~~ "}"
      }
    }
  }

  class ListOfDSLPairsRasterizer(pairs: List[DSLKeyValuePair]) extends Rasterizer {
    def rasterize(renderer: Renderer): Renderer = {
      if (pairs.isEmpty) {
        renderer ~~ "{}"
      } else {
        renderer ~~ "{"

        var isFirst = true
        pairs foreach {
          pair: DSLKeyValuePair =>
            if (isFirst) {
              isFirst = false
            } else {
              renderer ~~ ","
            }
            pair.key.recursiveRasterizer.rasterize(renderer)
            renderer ~~ ":"
            pair.datum.recursiveRasterizer.rasterize(renderer)
        }

        renderer ~~ "}"
      }
    }
  }

  private val base64Encoder = java.util.Base64.getEncoder

  class BinaryRasterizer(binary: ByteString) extends Rasterizer {
    def rasterize(renderer: Renderer): Renderer = {
      renderer ~~ """{"$reql_type$":"BINARY","data":""""
      renderer ~~ ByteString(base64Encoder.encode(binary.asByteBuffer))
      renderer ~~ """"}"""
    }
  }
}
