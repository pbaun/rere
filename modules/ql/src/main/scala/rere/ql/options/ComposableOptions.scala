package rere.ql.options

import rere.ql.ql2.Term.TermType
import rere.ql.rasterization.{recursive, trampolined}
import rere.ql.types.{ReqlExpr, ReqlObject}

trait ComposableOptions extends Options {
  protected def view: ComposableOptions.View
  protected def exprFromView: ComposableOptions.ReqlOptionsQuery =
    new ComposableOptions.ReqlOptionsQuery(view)
}

trait DefaultOption { this: ComposableOptions =>
  override final def isEmpty: Boolean = true
  override final val view: ComposableOptions.View = Nil
  override final val expr: ReqlObject = ComposableOptions.defaultExpr
}

trait NonDefaultOption { this: ComposableOptions =>
  override final def isEmpty: Boolean = false
  override final def expr: ReqlObject = exprFromView
}

object ComposableOptions {

  type View = List[(String, ReqlExpr)]

  def compose(options: ComposableOptions*): ComposableOptions = {
    new ComposableOptions {
      override def view: ComposableOptions.View = {
        val builder = List.newBuilder[(String, ReqlExpr)]
        options.foreach { option =>
          if (!option.isEmpty) {
            builder ++= option.view
          }
        }
        builder.result()
      }
      override def isEmpty: Boolean = options.forall(_.isEmpty)
      override def expr: ReqlObject = exprFromView
    }
  }

  private[options] final class ReqlOptionsQuery(val optionsView: View) extends ReqlObject {
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

  val defaultExpr: ReqlObject = new ComposableOptions.ReqlOptionsQuery(Nil)
}
