package rere.ql.options

import rere.ql.queries.values

trait ShardsOptions {

  sealed trait ShardsOptions extends ComposableOptions

  case object SingleShard extends ShardsOptions with DefaultOption

  case class Shards(amount: Int) extends ShardsOptions with NonDefaultOption {
    def view = "shards" -> values.expr(amount) :: Nil
  }

}
