package rere.ql.options

import rere.ql.queries.{Func, values}
import rere.ql.shapes.ModelShape
import rere.ql.typeclasses.Transmuter
import rere.ql.types.{PrimaryKey, ReqlModel}

trait ConflictOptions {

  sealed trait ConflictOptions[T, PK <: PrimaryKey] extends ComposableOptions

  case class ErrorOnConflict[T, PK <: PrimaryKey]() extends ConflictOptions[T, PK] with DefaultOption

  case class ReplaceOnConflict[T, PK <: PrimaryKey]() extends ConflictOptions[T, PK] with NonDefaultOption {
    def view = "conflict" -> values.expr("replace") :: Nil
  }

  case class UpdateOnConflict[T, PK <: PrimaryKey]() extends ConflictOptions[T, PK] with NonDefaultOption {
    def view = "conflict" -> values.expr("update") :: Nil
  }

  case class ResolveOnConflict[T, PK <: PrimaryKey](
    resolver: (PK#Reql, ReqlModel[T, PK], ReqlModel[T, PK]) => ReqlModel[T, PK]
  )(
    implicit shape: ModelShape[T, PK],
    tr: Transmuter[PK#Reql]
  ) extends ConflictOptions[T, PK] with NonDefaultOption {
    def view = "conflict" -> Func.wrap3(resolver) :: Nil
  }

}
