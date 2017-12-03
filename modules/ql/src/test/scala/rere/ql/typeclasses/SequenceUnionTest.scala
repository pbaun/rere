package rere.ql.typeclasses

import org.scalatest.FlatSpec
import rere.ql.types._

class SequenceUnionTest extends FlatSpec {

  //TODO: find the way to avoid implicit parameter and implement .union only with context bound
  //TODO: maybe need to define type alias inside .union class and make curried version of aux with single type parameter

  trait Out[T]

  implicit class UnionOnArrayOp[T <: ReqlDatum](val first: ReqlArray[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Other*)(
      implicit u: SequenceUnion.Aux[T, ReqlArray[T], Other, UnionOut]
    ): Out[UnionOut] = new Out[UnionOut] {}
  }

  implicit class UnionOnSelectionOfArrayOp[T, PK <: PrimaryKey](val first: ReqlSelectionOfArray[T, PK]) {
    def union[Other <: ReqlSequence[ReqlModel[T, PK]], UnionOut <: ReqlSequence[ReqlModel[T, PK]]](
      otherSeqs: Other*)(
      implicit u: SequenceUnion.Aux[ReqlModel[T, PK], ReqlSelectionOfArray[T, PK], Other, UnionOut]
    ): Out[UnionOut] = new Out[UnionOut] {}
  }

  implicit class UnionOnTableOp[T, PK <: PrimaryKey](val first: ReqlTable[T, PK]) {
    def union[Other <: ReqlSequence[ReqlModel[T, PK]], UnionOut <: ReqlSequence[ReqlModel[T, PK]]](
      otherSeqs: Other*)(
      implicit u: SequenceUnion.Aux[ReqlModel[T, PK], ReqlTable[T, PK], Other, UnionOut]
    ): Out[UnionOut] = new Out[UnionOut] {}
  }

  implicit class UnionOnTableSliceOp[T, PK <: PrimaryKey](val first: ReqlTableSlice[T, PK]) {
    def union[Other <: ReqlSequence[ReqlModel[T, PK]], UnionOut <: ReqlSequence[ReqlModel[T, PK]]](
      otherSeqs: Other*)(
      implicit u: SequenceUnion.Aux[ReqlModel[T, PK], ReqlTableSlice[T, PK], Other, UnionOut]
    ): Out[UnionOut] = new Out[UnionOut] {}
  }

  implicit class UnionOnSelectionOfStreamOp[T, PK <: PrimaryKey](val first: ReqlSelectionOfStream[T, PK]) {
    def union[Other <: ReqlSequence[ReqlModel[T, PK]], UnionOut <: ReqlSequence[ReqlModel[T, PK]]](
      otherSeqs: Other*)(
      implicit u: SequenceUnion.Aux[ReqlModel[T, PK], ReqlSelectionOfStream[T, PK], Other, UnionOut]
    ): Out[UnionOut] = new Out[UnionOut] {}
  }

  implicit class UnionOnFiniteStreamOp[T <: ReqlObject](val first: ReqlFiniteStream[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Other*)(
      implicit u: SequenceUnion.Aux[T, ReqlFiniteStream[T], Other, UnionOut]
    ): Out[UnionOut] = new Out[UnionOut] {}
  }

  implicit class UnionOnInfiniteStreamOp[T <: ReqlObject](val first: ReqlInfiniteStream[T]) {
    def union[Other <: ReqlSequence[T], UnionOut <: ReqlSequence[T]](
      otherSeqs: Other*)(
      implicit u: SequenceUnion.Aux[T, ReqlInfiniteStream[T], Other, UnionOut]
    ): Out[UnionOut] = new Out[UnionOut] {}
  }

  val array: ReqlArray[ReqlObject] = null
  val arrayOfModels: ReqlArray[ReqlModel[ReqlObject, PrimaryKey.UUID]] = null
  val table: ReqlTable[ReqlObject, PrimaryKey.UUID] = null
  val tableSlice: ReqlTableSlice[ReqlObject, PrimaryKey.UUID] = null
  val selectionOfArray: ReqlSelectionOfArray[ReqlObject, PrimaryKey.UUID] = null
  val selectionOfStream: ReqlSelectionOfStream[ReqlObject, PrimaryKey.UUID] = null
  val finiteStream: ReqlFiniteStream[ReqlObject] = null
  val finiteStreamOfModels: ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]] = null
  val infiniteStream: ReqlInfiniteStream[ReqlObject] = null
  val infiniteStreamOfModels: ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]] = null

  behavior of "SequenceUnion type class"

  it should "help to unite ReqlArray with other sequences" in {
    array.union()
    val res0_0: Out[ReqlArray[ReqlObject]] = array.union()

    array.union(array, array)
    val res0: Out[ReqlArray[ReqlObject]] = array.union(array, array)

    array.union(array, selectionOfArray)
    val res1: Out[ReqlArray[ReqlObject]] = array.union(array, selectionOfArray)
    val res2: Out[ReqlArray[ReqlObject]] = array.union(selectionOfArray, selectionOfArray)
    val res2_1: Out[ReqlArray[ReqlObject]] = array.union(array)
    val res2_2: Out[ReqlArray[ReqlObject]] = array.union(selectionOfArray)

    array.union(array, selectionOfStream)
    val res3: Out[ReqlFiniteStream[ReqlObject]] = array.union(array, selectionOfStream)

    array.union(selectionOfArray, selectionOfStream)
    val res4: Out[ReqlFiniteStream[ReqlObject]] = array.union(selectionOfArray, selectionOfStream)
    val res5: Out[ReqlFiniteStream[ReqlObject]] = array.union(selectionOfStream, selectionOfStream)
    val res5_1: Out[ReqlFiniteStream[ReqlObject]] = array.union(selectionOfStream)
    val res5_2: Out[ReqlFiniteStream[ReqlObject]] = array.union(table, selectionOfStream)
    val res5_3: Out[ReqlFiniteStream[ReqlObject]] = array.union(table)
    val res5_4: Out[ReqlFiniteStream[ReqlObject]] = array.union(tableSlice)
    val res5_5: Out[ReqlFiniteStream[ReqlObject]] = array.union(table, tableSlice)
    val res5_6: Out[ReqlFiniteStream[ReqlObject]] = array.union(array, table, tableSlice)

    val res6: Out[ReqlFiniteStream[ReqlObject]] = array.union(array, finiteStream)
    val res7: Out[ReqlFiniteStream[ReqlObject]] = array.union(finiteStream, selectionOfArray)
    val res8: Out[ReqlFiniteStream[ReqlObject]] = array.union(finiteStream, finiteStream)
    val res8_1: Out[ReqlFiniteStream[ReqlObject]] = array.union(finiteStream)

    val res9: Out[ReqlInfiniteStream[ReqlObject]] = array.union(array, infiniteStream)
    val res10: Out[ReqlInfiniteStream[ReqlObject]] = array.union(finiteStream, infiniteStream)
    val res11: Out[ReqlInfiniteStream[ReqlObject]] = array.union(infiniteStream, infiniteStream)
    val res12: Out[ReqlInfiniteStream[ReqlObject]] = array.union(infiniteStream)
  }

  it should "help to unite ReqlSelectionOfArray with other sequences" in {
    val res13: Out[ReqlArray[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union()
    val res14: Out[ReqlArray[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(selectionOfArray)
    val res15: Out[ReqlArray[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(selectionOfArray, selectionOfArray)
    val res16: Out[ReqlArray[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(selectionOfArray, arrayOfModels)

    val res17: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(arrayOfModels, selectionOfStream)
    val res18: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(selectionOfArray, selectionOfStream)
    val res19: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(arrayOfModels, finiteStreamOfModels)
    val res20: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(selectionOfStream)
    val res21: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(finiteStreamOfModels)

    val res22: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(infiniteStreamOfModels)
    val res23: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(arrayOfModels, infiniteStreamOfModels)
    val res24: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfArray.union(finiteStreamOfModels, infiniteStreamOfModels)
  }

  it should "help to unite ReqlTable with other sequences" in {
    val res25: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union()
    val res26: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union(arrayOfModels)
    val res27: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union(table)
    val res28: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union(table, tableSlice)
    val res29: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union(finiteStreamOfModels)
    val res30: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union(finiteStreamOfModels, selectionOfStream)
    val res31: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union(arrayOfModels, table, tableSlice, selectionOfArray, selectionOfStream, finiteStreamOfModels)

    val res32: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union(infiniteStreamOfModels)
    val res33: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union(infiniteStreamOfModels, table)
    val res34: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = table.union(infiniteStreamOfModels, finiteStreamOfModels)
  }

  it should "help to unite ReqlTableSlice with other sequences" in {
    tableSlice.union()
    val res35: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union()

    tableSlice.union(arrayOfModels)
    val res37: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(arrayOfModels)

    tableSlice.union(table)
    val res38: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(table)

    tableSlice.union(tableSlice)
    val res39: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(tableSlice)
    val res40: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(table, tableSlice)
    val res41: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(finiteStreamOfModels)
    val res42: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(finiteStreamOfModels, selectionOfStream)
    val res43: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(arrayOfModels, table, tableSlice, selectionOfArray, selectionOfStream, finiteStreamOfModels)

    val res44: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(infiniteStreamOfModels)
    val res45: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(infiniteStreamOfModels, table)
    val res46: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = tableSlice.union(infiniteStreamOfModels, finiteStreamOfModels)
  }

  it should "help to unite ReqlSelectionOfStream with other sequences" in {
    val res47: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfStream.union()
    selectionOfStream.union(arrayOfModels)  // Universal instances not working in that case
    val res48: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfStream.union(arrayOfModels)
    val res49: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfStream.union(table)
    val res50: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfStream.union(tableSlice)
    val res51: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfStream.union(finiteStreamOfModels)
    val res52: Out[ReqlFiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfStream.union(arrayOfModels, table, tableSlice, selectionOfArray, selectionOfStream, finiteStreamOfModels)

    selectionOfStream.union(infiniteStreamOfModels)
    val res53: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfStream.union(infiniteStreamOfModels)
    selectionOfStream.union(infiniteStreamOfModels, table)
    val res54: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfStream.union(infiniteStreamOfModels, table)
    selectionOfStream.union(infiniteStreamOfModels, finiteStreamOfModels)
    val res55: Out[ReqlInfiniteStream[ReqlModel[ReqlObject, PrimaryKey.UUID]]] = selectionOfStream.union(infiniteStreamOfModels, finiteStreamOfModels)
  }

  it should "help to unite ReqlFiniteStream with other sequences" in {
    finiteStream.union()
    val res56: Out[ReqlFiniteStream[ReqlObject]] = finiteStream.union()
    val res57: Out[ReqlFiniteStream[ReqlObject]] = finiteStream.union(array)
    val res58: Out[ReqlFiniteStream[ReqlObject]] = finiteStream.union(table)
    val res59: Out[ReqlFiniteStream[ReqlObject]] = finiteStream.union(tableSlice)
    finiteStream.union(finiteStream)
    val res60: Out[ReqlFiniteStream[ReqlObject]] = finiteStream.union(finiteStream)
    val res61: Out[ReqlFiniteStream[ReqlObject]] = finiteStream.union(array, table, tableSlice, selectionOfArray, selectionOfStream, finiteStream)

    finiteStream.union(infiniteStream)
    val res62: Out[ReqlInfiniteStream[ReqlObject]] = finiteStream.union(infiniteStream)
    val res63: Out[ReqlInfiniteStream[ReqlObject]] = finiteStream.union(infiniteStream, table)
    val res64: Out[ReqlInfiniteStream[ReqlObject]] = finiteStream.union(infiniteStream, finiteStream)
  }

  it should "help to unite ReqlInfiniteStream with other sequences" in {
    infiniteStream.union()
    val res65: Out[ReqlInfiniteStream[ReqlObject]] = infiniteStream.union()
    val res66: Out[ReqlInfiniteStream[ReqlObject]] = infiniteStream.union(array)
    val res67: Out[ReqlInfiniteStream[ReqlObject]] = infiniteStream.union(table)
    val res68: Out[ReqlInfiniteStream[ReqlObject]] = infiniteStream.union(tableSlice)
    val res69: Out[ReqlInfiniteStream[ReqlObject]] = infiniteStream.union(finiteStream)
    val res70: Out[ReqlInfiniteStream[ReqlObject]] = infiniteStream.union(array, table, tableSlice, selectionOfArray, selectionOfStream, finiteStream)
    infiniteStream.union(infiniteStream)
    val res71: Out[ReqlInfiniteStream[ReqlObject]] = infiniteStream.union(infiniteStream)
    val res72: Out[ReqlInfiniteStream[ReqlObject]] = infiniteStream.union(infiniteStream, table)
    val res73: Out[ReqlInfiniteStream[ReqlObject]] = infiniteStream.union(infiniteStream, finiteStream)
  }
}
