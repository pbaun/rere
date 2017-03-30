package rere.ql.options

import rere.ql.queries.{Func, values}
import rere.ql.shapes.{ModelShape, ReqlModel}
import rere.ql.types.ReqlDatum

trait ConflictOptions {

  sealed trait ConflictOptions[T, PK] extends ComposableOptions

  case class ErrorOnConflict[T, PK]() extends ConflictOptions[T, PK] {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class ReplaceOnConflict[T, PK]() extends ConflictOptions[T, PK] {
    def isEmpty = false
    def view = "conflict" -> values.expr("replace") :: Nil
    val innerQuery = query
  }

  case class UpdateOnConflict[T, PK]() extends ConflictOptions[T, PK] {
    def isEmpty = false
    def view = "conflict" -> values.expr("update") :: Nil
    val innerQuery = query
  }

  //TODO: id type
  case class ResolveOnConflict[T, PK](
    resolver: (ReqlDatum, ReqlModel[T, PK], ReqlModel[T, PK]) => ReqlModel[T, PK]
  )(
    implicit shape: ModelShape[T, PK]
  ) extends ConflictOptions[T, PK] {
    def isEmpty = false
    def view = "conflict" -> Func.wrap3(resolver) :: Nil
    val innerQuery = query
  }

}
