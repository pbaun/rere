package rere.ql.options

import rere.ql.ql2.Term.TermType
import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.types.{ReqlExpr, ReqlObject}

trait ComposableOptions extends Options {
  protected def view: ComposableOptions.View
  protected def exprFromView: ComposableOptions.ReqlOptionsQuery =
    new ComposableOptions.ReqlOptionsQuery(view)
}

object ComposableOptions {

  type View = List[(String, ReqlExpr)]

  def compose(options: ComposableOptions*): ComposableOptions = {
    new ComposableOptions {
      def view = {
        val builder = List.newBuilder[(String, ReqlExpr)]
        options.foreach { option =>
          if (!option.isEmpty) {
            builder ++= option.view
          }
        }
        builder.result()
      }
      def isEmpty = options.forall(_.isEmpty)
      def expr = exprFromView
    }
  }

  private[options] class ReqlOptionsQuery(val optionsView: View) extends ReqlObject {
    def string = "make_obj"
    def command = TermType.MAKE_OBJ
    def arguments = Nil
    def options = Options.empty

    override def recursiveRasterizer: recursive.Rasterizer = {
      new recursive.ListOfPairsRasterizer(optionsView)
    }

    override def trampolinedRasterizer: trampolined.Rasterizer = {
      new trampolined.ListOfPairsRasterizer(optionsView)
    }
  }
}
