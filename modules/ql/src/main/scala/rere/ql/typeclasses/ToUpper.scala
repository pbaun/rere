package rere.ql.typeclasses

import rere.ql.shapes.ModelShape
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

  implicit def tableTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Projection[ReqlTable[T, PK]] = new ToUpper[ReqlTable[T, PK]] {
    final type UpperType = ReqlTable[T, PK]
    def toUpper(query: ReqlExpr): ReqlTable[T, PK] = Transmuter.tableTransmuter.transmute(query)
  }

  implicit def tableSliceTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Projection[ReqlTableSlice[T, PK]] = new ToUpper[ReqlTableSlice[T, PK]] {
    final type UpperType = ReqlTableSlice[T, PK]
    def toUpper(query: ReqlExpr): ReqlTableSlice[T, PK] = Transmuter.tableSliceTransmuter.transmute(query)
  }

  implicit def selectionOfArrayTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Projection[ReqlSelectionOfArray[T, PK]] = new ToUpper[ReqlSelectionOfArray[T, PK]] {
    final type UpperType = ReqlSelectionOfArray[T, PK]
    def toUpper(query: ReqlExpr): ReqlSelectionOfArray[T, PK] = Transmuter.selectionOfArrayTransmuter.transmute(query)
  }

  implicit def selectionOfStreamTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Projection[ReqlSelectionOfStream[T, PK]] = new ToUpper[ReqlSelectionOfStream[T, PK]] {
    final type UpperType = ReqlSelectionOfStream[T, PK]
    def toUpper(query: ReqlExpr): ReqlSelectionOfStream[T, PK] = Transmuter.selectionOfStreamTransmuter.transmute(query)
  }

  implicit def selectionOfObjectTransmuter[T, PK](
    implicit modelShape: ModelShape[T, PK]
  ): Projection[ReqlSelectionOfObject[T, PK]] = new ToUpper[ReqlSelectionOfObject[T, PK]] {
    final type UpperType = ReqlSelectionOfObject[T, PK]
    def toUpper(query: ReqlExpr): ReqlSelectionOfObject[T, PK] = Transmuter.selectionOfObjectTransmuter.transmute(query)
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
