package rere.ql.queries

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types.{ReqlArray, ReqlDatum, ReqlInteger}

trait ArrayQueries {

  // append
  trait AppendArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  //TODO: check inferred type

  implicit class AppendOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def append[U >: T <: ReqlDatum](value: U): AppendArrayQuery[U] = new AppendArrayQuery[U] {
      val command = TermType.APPEND
      val string = "append"
      val arguments = array :: value :: Nil
      val options = Options.empty
    }
  }

  // prepend
  trait PrependArrayQuery[T <: ReqlDatum] extends ReqlArray[T]
  //TODO: check inferred type

  implicit class PrependOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def prepend[U >: T <: ReqlDatum](value: U): PrependArrayQuery[U] = new PrependArrayQuery[U] {
      val command = TermType.PREPEND
      val string = "prepend"
      val arguments = array :: value :: Nil
      val options = Options.empty
    }
  }

  // difference
  trait DifferenceArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class DifferenceOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def difference(otherArray: ReqlArray[T]): DifferenceArrayQuery[T] = new DifferenceArrayQuery[T] {
      val command = TermType.DIFFERENCE
      val string = "difference"
      val arguments = array :: otherArray :: Nil
      val options = Options.empty
    }
  }

  // set_insert
  trait SetInsertArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class SetInsertOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def setInsert(value: T): SetInsertArrayQuery[T] = new SetInsertArrayQuery[T] {
      val command = TermType.SET_INSERT
      val string = "set_insert"
      val arguments = array :: value :: Nil
      val options = Options.empty
    }
  }

  // set_union
  trait SetUnionArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class SetUnionOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def setUnion(otherArray: ReqlArray[T]): SetUnionArrayQuery[T] = new SetUnionArrayQuery[T] {
      val command = TermType.SET_UNION
      val string = "set_union"
      val arguments = array :: otherArray :: Nil
      val options = Options.empty
    }
  }

  // set_intersection
  trait SetIntersectionArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class SetIntersectionOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def setIntersection(otherArray: ReqlArray[T]): SetIntersectionArrayQuery[T] = new SetIntersectionArrayQuery[T] {
      val command = TermType.SET_INTERSECTION
      val string = "set_intersection"
      val arguments = array :: otherArray :: Nil
      val options = Options.empty
    }
  }

  // set_difference
  trait SetDifferenceArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class SetDifferenceOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def setDifference(otherArray: ReqlArray[T]): SetDifferenceArrayQuery[T] = new SetDifferenceArrayQuery[T] {
      val command = TermType.SET_DIFFERENCE
      val string = "set_difference"
      val arguments = array :: otherArray :: Nil
      val options = Options.empty
    }
  }

  // insert_at
  trait InsertAtArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class InsertAtOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def insertAt[U >: T <: ReqlDatum](offset: ReqlInteger, value: U): InsertAtArrayQuery[U] = new InsertAtArrayQuery[U] {
      val command = TermType.INSERT_AT
      val string = "insert_at"
      val arguments = array :: offset :: value :: Nil
      val options = Options.empty
    }
  }

  // splice_at
  trait SpliceAtArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class SpliceAtOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def spliceAt[U >: T <: ReqlDatum](offset: ReqlInteger, insertedArray: ReqlArray[U]): SpliceAtArrayQuery[U] = new SpliceAtArrayQuery[U] {
      val command = TermType.SPLICE_AT
      val string = "splice_at"
      val arguments = array :: offset :: insertedArray :: Nil
      val options = Options.empty
    }
  }

  // delete_at
  trait DeleteAtArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class DeleteAtOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def deleteAt(offset: ReqlInteger): DeleteAtArrayQuery[T] = new DeleteAtArrayQuery[T] {
      val command = TermType.DELETE_AT
      val string = "delete_at"
      val arguments = array :: offset :: Nil
      val options = Options.empty
    }

    def deleteAt(offset: ReqlInteger, endOffset: ReqlInteger): DeleteAtArrayQuery[T] = new DeleteAtArrayQuery[T] {
      val command = TermType.DELETE_AT
      val string = "delete_at"
      val arguments = array :: offset :: endOffset :: Nil
      val options = Options.empty
    }
  }

  // change_at
  trait ChangeAtArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class ChangeAtOnArrayOp[T <: ReqlDatum](val array: ReqlArray[T]) {
    def changeAt[U >: T <: ReqlDatum](offset: ReqlInteger, value: U): ChangeAtArrayQuery[U] = new ChangeAtArrayQuery[U] {
      val command = TermType.CHANGE_AT
      val string = "change_at"
      val arguments = array :: offset :: value :: Nil
      val options = Options.empty
    }
  }

}
