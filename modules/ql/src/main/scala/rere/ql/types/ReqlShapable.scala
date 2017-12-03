package rere.ql.types

import rere.ql.shapes.ModelShape

trait ReqlShapable[T, PK <: PrimaryKey] {
  def shape: ModelShape[T, PK]
}
