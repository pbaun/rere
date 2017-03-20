package rere.driver.protocol

import java.nio.charset.StandardCharsets

import akka.util.ByteString
import rere.ql.options.Options
import rere.ql.ql2
import rere.ql.rasterization.{ByteStringRenderer, Renderer}
import rere.ql.types._

sealed trait ReqlCommand {
  def commandType: Int
  def render(r: Renderer): Renderer
}

object ReqlCommand {
  def render(command: ReqlCommand): ByteString = {
    val renderer = new ByteStringRenderer(StandardCharsets.UTF_8)
    command.render(renderer).get
  }
}

final class ReqlStartCommand[T <: ReqlExpr](val expression: T, val options: Options) extends ReqlCommand {
  import cats.instances.function._

  override def commandType = ql2.Query.QueryType.START

  override def render(renderer: Renderer): Renderer = {
    val r1 = renderer ~~ "[" ~~ commandType.toString ~~ ","
    val r2 = expression.trampolinedRasterizer.rasterize(r1).run ~~ ","
    options.trampolinedRasterizer.rasterize(r2).run ~~ "]"
  }
}

final class ReqlContinueCommand extends ReqlCommand {
  override def commandType = ql2.Query.QueryType.CONTINUE

  override def render(renderer: Renderer): Renderer = {
    renderer ~~ "[" ~~ commandType.toString ~~ "]"
  }
}

final class ReqlStopCommand extends ReqlCommand {
  override def commandType = ql2.Query.QueryType.STOP

  override def render(renderer: Renderer): Renderer = {
    renderer ~~ "[" ~~ commandType.toString ~~ "]"
  }
}
