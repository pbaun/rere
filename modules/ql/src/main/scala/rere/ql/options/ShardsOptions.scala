package rere.ql.options

import rere.ql.queries.values

trait ShardsOptions {

  sealed trait ShardsOptions extends ComposableOptions

  case object SingleShard extends ShardsOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class Shards(amount: Int) extends ShardsOptions {
    def isEmpty = false
    def view = "shards" -> values.expr(amount) :: Nil
    def expr = exprFromView
  }

}
