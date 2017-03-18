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
    def rasterize(): Trampoline[Unit]
  }

  class QueryRasterizer(renderer: Renderer, query: ReqlExpr) extends Rasterizer {
    def rasterize(): Trampoline[Unit] = {
      def rasterizeArgs(): Trampoline[Unit] = {
        query.arguments.zipWithIndex.traverse[Trampoline, Unit] {
          case (arg, index) =>
            if (index == 0) {
              arg.getTrampolinedRasterizer(renderer).rasterize()
            } else {
              for {
                _ <- delay { renderer ~~ "," }
                _ <- arg.getTrampolinedRasterizer(renderer).rasterize()
              } yield ()
            }
        } map { _ => () }
      }

      def rasterizeOptions(): Trampoline[Unit] = {
        query.options.getTrampolinedRasterizer(renderer).rasterize()
      }

      for {
        _ <- delay { renderer ~~ "[" ~~ query.command.toString ~~ ",[" }
        _ <- rasterizeArgs()
        _ <- delay { renderer ~~ "]" }
        _ <- suspend {
          if (!query.options.isEmpty) {
            for {
              _ <- delay { renderer ~~ "," }
              _ <- rasterizeOptions()
            } yield ()
          } else {
            done(())
          }
        }
        _ <- delay { renderer ~~ "]" }
      } yield ()
    }
  }

  class PrimitiveRasterizer(renderer: Renderer, representation: String) extends Rasterizer {
    def rasterize(): Trampoline[Unit] = {
      delay {
        renderer ~~ representation
        ()
      }
    }
  }

  class ObjRasterizer(renderer: Renderer, obj: Map[String, ReqlExpr]) extends Rasterizer {

    private def encloseJsonString(value: String): String = Json.fromString(value).noSpaces

    def rasterize(): Trampoline[Unit] = {
      for {
        _ <- delay { renderer ~~ "{" }
        _ <- obj.toList.zipWithIndex.traverse[Trampoline, Unit] {
          case ((key, value), index) =>
            val comma = if (index == 0) {
              done(())
            } else {
              delay[Unit] {
                renderer ~~ ","
                ()
              }
            }
            for {
              _ <- comma
              _ <- delay { renderer ~~ encloseJsonString(key) ~~ ":" }
              _ <- value.getTrampolinedRasterizer(renderer).rasterize()
            } yield ()
        }
        _ <- delay { renderer ~~ "}" }
      } yield ()
    }
  }

  class ListOfPairsRasterizer(renderer: Renderer, pairs: List[(String, ReqlExpr)]) extends Rasterizer {

    private def encloseJsonString(value: String): String = Json.fromString(value).noSpaces

    def rasterize(): Trampoline[Unit] = {
      if (pairs.isEmpty) {
        done {
          renderer ~~ "{}"
          ()
        }
      } else {
        for {
          _ <- delay {
            renderer ~~ "{"
            ()
          }
          _ <- pairs.zipWithIndex.traverse[Trampoline, Unit] {
            case ((key, value), index) =>
              val comma = if (index == 0) {
                done(())
              } else {
                delay[Unit] {
                  renderer ~~ ","
                  ()
                }
              }
              for {
                _ <- comma
                _ <- delay { renderer ~~ encloseJsonString(key) ~~ ":" }
                _ <- value.getTrampolinedRasterizer(renderer).rasterize()
              } yield ()
          }
          _ <- delay { renderer ~~ "}" }
        } yield ()
      }
    }
  }

  class ListOfDSLPairsRasterizer(renderer: Renderer, pairs: List[DSLKeyValuePair]) extends Rasterizer {

    def rasterize(): Trampoline[Unit] = {
      if (pairs.isEmpty) {
        done {
          renderer ~~ "{}"
          ()
        }
      } else {
        for {
          _ <- delay {
            renderer ~~ "{"
            ()
          }
          _ <- pairs.zipWithIndex.traverse[Trampoline, Unit] {
            case (pair, index) =>
              val comma = if (index == 0) {
                done(())
              } else {
                delay[Unit] {
                  renderer ~~ ","
                  ()
                }
              }
              for {
                _ <- comma
                _ <- pair.key.getTrampolinedRasterizer(renderer).rasterize()
                _ <- delay { renderer ~~ ":" }
                _ <- pair.datum.getTrampolinedRasterizer(renderer).rasterize()
              } yield ()
          }
          _ <- delay { renderer ~~ "}" }
        } yield ()
      }
    }
  }

  private val base64Encoder = java.util.Base64.getEncoder

  class BinaryRasterizer(renderer: Renderer, binary: ByteString) extends Rasterizer {
    def rasterize(): Trampoline[Unit] = {
      for {
        _ <- delay { renderer ~~ """{"$reql_type$":"BINARY","data":"""" }
        _ <- delay { renderer ~~ ByteString(base64Encoder.encode(binary.asByteBuffer)) }
        _ <- delay { renderer ~~ """"}""" }
      } yield ()
    }
  }
}
