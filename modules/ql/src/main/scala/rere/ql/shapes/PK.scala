package rere.ql.shapes

import rere.ql.types.PrimaryKey

sealed trait PK[Key <: PrimaryKey]

object PK {
  def apply[Key <: PrimaryKey]: PK[Key] = new PK[Key] {}
}
