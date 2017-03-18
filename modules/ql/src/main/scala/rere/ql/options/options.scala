package rere.ql.options

import rere.ql.ql2.Term.TermType
import rere.ql.rasterization.{Renderer, recursive, trampolined}
import rere.ql.types.{ReqlExpr, ReqlObject, ReqlPrimitiveExpr}

import scala.collection.mutable

//TODO: make test for checking keys format (should be snake_case)
trait Options {
  def isEmpty: Boolean
  def innerQuery: ReqlExpr

  def getRasterizer(renderer: Renderer): recursive.Rasterizer = {
    innerQuery.getRasterizer(renderer)
  }
  def getTrampolinedRasterizer(renderer: Renderer): trampolined.Rasterizer = {
    innerQuery.getTrampolinedRasterizer(renderer)
  }
}

object Options {
  object EmptyOptions extends Options {
    override def isEmpty = true
    override val innerQuery: ReqlExpr = new ReqlPrimitiveExpr {
      def string: String = "obj"
      def repr = "{}"
    }
  }

  val empty: Options = EmptyOptions
}

trait ComposableOptions extends Options {
  protected def view: ComposableOptions.View
  protected def query: ComposableOptions.ReqlOptionsQuery =
    new ComposableOptions.ReqlOptionsQuery(view)
}

object ComposableOptions {

  type View = List[(String, ReqlExpr)]

  def compose(options: ComposableOptions*): ComposableOptions = {
    new ComposableOptions {
      val view = {
        val buffer = new mutable.ListBuffer[(String, ReqlExpr)]
        options.foreach(buffer ++= _.view)
        buffer.toList
      }
      def isEmpty = view.isEmpty
      def innerQuery = query
    }
  }

  private[options] class ReqlOptionsQuery(val optionsView: View) extends ReqlObject {

    def isEmpty = optionsView.isEmpty

    def string = "make_obj"
    def command = TermType.MAKE_OBJ
    def arguments = Nil
    def options = Options.empty

    override def getRasterizer(renderer: Renderer): recursive.Rasterizer = {
      new recursive.ListOfPairsRasterizer(renderer, optionsView)
    }

    override def getTrampolinedRasterizer(renderer: Renderer): trampolined.Rasterizer = {
      new trampolined.ListOfPairsRasterizer(renderer, optionsView)
    }
  }
}
