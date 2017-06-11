package rere.ql.options

import rere.ql.queries.values

trait PrimaryKeyOptions {

  sealed trait PrimaryKeyOptions extends ComposableOptions

  case object DefaultPrimaryKey extends PrimaryKeyOptions with DefaultOption

  case class PrimaryKey(primaryKey: String) extends PrimaryKeyOptions with NonDefaultOption {
    def view = "primary_key" -> values.expr(primaryKey) :: Nil
  }

}
