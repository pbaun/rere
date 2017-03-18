package rere.ql.options

import rere.ql.queries.values

trait DurabilityOptions {

  sealed trait DurabilityOptions extends ComposableOptions

  //maybe QueryDurability (durability level defined by .run)
  case object DefaultDurability extends DurabilityOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object Hard extends DurabilityOptions {
    def isEmpty = false
    def view = "durability" -> values.expr("hard") :: Nil
    val innerQuery = query
  }

  case object Soft extends DurabilityOptions {
    def isEmpty = false
    def view = "durability" -> values.expr("soft") :: Nil
    val innerQuery = query
  }

}
