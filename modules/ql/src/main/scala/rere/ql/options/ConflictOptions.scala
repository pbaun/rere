package rere.ql.options

import rere.ql.queries.{Func, values}
import rere.ql.typeclasses.Transmuter
import rere.ql.types.{ReqlDatum, ReqlObject}

trait ConflictOptions {

  sealed trait ConflictOptions[+T <: ReqlObject] extends ComposableOptions

  case object ErrorOnConflict extends ConflictOptions[Nothing] {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case object ReplaceOnConflict extends ConflictOptions[Nothing] {
    def isEmpty = false
    def view = "conflict" -> values.expr("replace") :: Nil
    val innerQuery = query
  }

  case object UpdateOnConflict extends ConflictOptions[Nothing] {
    def isEmpty = false
    def view = "conflict" -> values.expr("update") :: Nil
    val innerQuery = query
  }

  //TODO: id type
  case class ResolveOnConflict[T <: ReqlObject : Transmuter](resolver: (ReqlDatum, T, T) => T) extends ConflictOptions[T] {
    def isEmpty = false
    def view = "conflict" -> Func.wrap3(resolver) :: Nil
    val innerQuery = query
  }

}
