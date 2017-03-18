package rere.ql.options

import rere.ql.queries.values

trait PrimaryKeyOptions {

  sealed trait PrimaryKeyOptions extends ComposableOptions

  case object DefaultPrimaryKey extends PrimaryKeyOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class PrimaryKey(primaryKey: String) extends PrimaryKeyOptions {
    def isEmpty = false
    def view = "primary_key" -> values.expr(primaryKey) :: Nil
    def innerQuery = query
  }

}
