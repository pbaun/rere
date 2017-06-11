package rere.ql.options

import rere.ql.queries.values

trait DurabilityOptions {

  sealed trait DurabilityOptions extends ComposableOptions

  //maybe QueryDurability (durability level defined by .run)
  case object DefaultDurability extends DurabilityOptions with DefaultOption

  case object Hard extends DurabilityOptions with NonDefaultOption {
    def view = "durability" -> values.expr("hard") :: Nil
  }

  case object Soft extends DurabilityOptions with NonDefaultOption {
    def view = "durability" -> values.expr("soft") :: Nil
  }

}
