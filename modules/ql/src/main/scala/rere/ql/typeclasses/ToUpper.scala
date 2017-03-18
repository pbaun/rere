package rere.ql.typeclasses

import rere.ql.types._

import scala.annotation.implicitNotFound

/**
  * Special type class for .do query. Basically it used for casting type to upper bound aka SimpleType
  *
  *  @tparam From Type of input. It should be contravariant because instance for ReqlTable should be
  *               found when compiler will search instance for TableQuery.
  * */
@implicitNotFound("Type ${From} can't be used as input of .do query (very likely it's a bug)")
trait ToUpper[-From] {
  type UpperType <: ReqlExpr
  def toUpper(query: ReqlExpr): UpperType
}

object ToUpper extends LowPriorityDoTransmuter {

  type Aux[-T, U] = ToUpper[T] { type UpperType = U }

  type Projection[T] = ToUpper.Aux[T, T]

  def apply[T](query: ReqlExpr)(implicit toT: ToUpper[T]): toT.UpperType = toT.toUpper(query)

  def toT[T](query: ReqlExpr)(implicit toT: Aux[T, T]): T = toT.toUpper(query)

  implicit val booleanTransmuter: Projection[ReqlBoolean] = new ToUpper[ReqlBoolean] {
    final type UpperType = ReqlBoolean
    def toUpper(query: ReqlExpr): ReqlBoolean = Transmuter.booleanTransmuter.transmute(query)
  }

  implicit def tableTransmuter[T <: ReqlObject]: Projection[ReqlTable[T]] = new ToUpper[ReqlTable[T]] {
    final type UpperType = ReqlTable[T]
    def toUpper(query: ReqlExpr): ReqlTable[T] = Transmuter.tableTransmuter.transmute(query)
  }

  implicit def tableSliceTransmuter[T <: ReqlObject]: Projection[ReqlTableSlice[T]] = new ToUpper[ReqlTableSlice[T]] {
    final type UpperType = ReqlTableSlice[T]
    def toUpper(query: ReqlExpr): ReqlTableSlice[T] = Transmuter.tableSliceTransmuter.transmute(query)
  }

  implicit def selectionOfArrayTransmuter[T <: ReqlObject]: Projection[ReqlSelectionOfArray[T]] = new ToUpper[ReqlSelectionOfArray[T]] {
    final type UpperType = ReqlSelectionOfArray[T]
    def toUpper(query: ReqlExpr): ReqlSelectionOfArray[T] = Transmuter.selectionOfArrayTransmuter.transmute(query)
  }

  implicit def selectionOfStreamTransmuter[T <: ReqlObject]: Projection[ReqlSelectionOfStream[T]] = new ToUpper[ReqlSelectionOfStream[T]] {
    final type UpperType = ReqlSelectionOfStream[T]
    def toUpper(query: ReqlExpr): ReqlSelectionOfStream[T] = Transmuter.selectionOfStreamTransmuter.transmute(query)
  }

  implicit def selectionOfObjectTransmuter[T <: ReqlObject]: Projection[ReqlSelectionOfObject[T]] = new ToUpper[ReqlSelectionOfObject[T]] {
    final type UpperType = ReqlSelectionOfObject[T]
    def toUpper(query: ReqlExpr): ReqlSelectionOfObject[T] = Transmuter.selectionOfObjectTransmuter.transmute(query)
  }

  implicit def finiteStreamTransmuter[T <: ReqlDatum]: Projection[ReqlFiniteStream[T]] = new ToUpper[ReqlFiniteStream[T]] {
    final type UpperType = ReqlFiniteStream[T]
    def toUpper(query: ReqlExpr): ReqlFiniteStream[T] = Transmuter.finiteStreamTransmuter.transmute(query)
  }

  implicit def infiniteStreamTransmuter[T <: ReqlDatum]: Projection[ReqlInfiniteStream[T]] = new ToUpper[ReqlInfiniteStream[T]] {
    final type UpperType = ReqlInfiniteStream[T]
    def toUpper(query: ReqlExpr): ReqlInfiniteStream[T] = Transmuter.infiniteStreamTransmuter.transmute(query)
  }
}

trait LowPriorityDoTransmuter {
  implicit def fromTransmuter[T <: ReqlExpr : Transmuter]: ToUpper.Projection[T] = new ToUpper[T] {
    final type UpperType = T
    override def toUpper(query: ReqlExpr): UpperType = Transmuter.transmute[T](query)
  }
}
