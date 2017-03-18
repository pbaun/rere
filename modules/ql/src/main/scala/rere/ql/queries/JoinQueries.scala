package rere.ql.queries

import rere.ql.options.all._
import rere.ql.options.{ComposableOptions, Options}
import rere.ql.ql2.Term.TermType
import rere.ql.typeclasses.Transmuter
import rere.ql.types._

trait JoinQueries {

  // inner_join
  trait InnerJoinInfiniteStreamLikeQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait InnerJoinFiniteStreamLikeQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait InnerJoinArrayLikeQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class InnerJoinOnInfiniteStreamLikeOp[T <: ReqlValue : Transmuter](val infiniteStreamLike: ReqlInfiniteStreamLike[T]) {
    def innerJoin[
      U <: ReqlValue : Transmuter
    ](
      otherSeq: ReqlFiniteSequence[U],
      predicate: (T, U) => ReqlBoolean
    ): InnerJoinInfiniteStreamLikeQuery[ReqlJoinResult[T, U]] = new InnerJoinInfiniteStreamLikeQuery[ReqlJoinResult[T, U]] {
      val command = TermType.INNER_JOIN
      val string = "inner_join"
      val arguments = infiniteStreamLike :: otherSeq :: Func.wrap2(predicate) :: Nil
      val options = Options.empty
    }
  }

  implicit class InnerJoinOnFiniteStreamLikeOp[T <: ReqlValue : Transmuter](val finiteStreamLike: ReqlFiniteStreamLike[T]) {
    def innerJoin[
      U <: ReqlValue : Transmuter
    ](
      otherSeq: ReqlFiniteSequence[U],
      predicate: (T, U) => ReqlBoolean
    ): InnerJoinFiniteStreamLikeQuery[ReqlJoinResult[T, U]] = new InnerJoinFiniteStreamLikeQuery[ReqlJoinResult[T, U]] {
      val command = TermType.INNER_JOIN
      val string = "inner_join"
      val arguments = finiteStreamLike :: otherSeq :: Func.wrap2(predicate) :: Nil
      val options = Options.empty
    }
  }

  implicit class InnerJoinOnArrayLikeOp[T <: ReqlDatum : Transmuter](val arrayLike: ReqlFiniteArrayLike[T]) {
    def innerJoin[
      U <: ReqlDatum : Transmuter
    ](
      otherSeq: ReqlFiniteSequence[U],
      predicate: (T, U) => ReqlBoolean
    ): InnerJoinArrayLikeQuery[ReqlJoinResult[T, U]] = new InnerJoinArrayLikeQuery[ReqlJoinResult[T, U]] {
      val command = TermType.INNER_JOIN
      val string = "inner_join"
      val arguments = arrayLike :: otherSeq :: Func.wrap2(predicate) :: Nil
      val options = Options.empty
    }
  }

  // outer_join
  trait OuterJoinInfiniteStreamLikeQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait OuterJoinFiniteStreamLikeQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait OuterJoinArrayLikeQuery[T <: ReqlDatum] extends ReqlArray[T]

  implicit class OuterJoinOnInfiniteStreamLikeOp[T <: ReqlDatum : Transmuter](val infiniteStreamLike: ReqlInfiniteStreamLike[T]) {
    def outerJoin[
      U <: ReqlDatum : Transmuter
    ](
      otherSeq: ReqlFiniteSequence[U],
      predicate: (T, U) => ReqlBoolean
    ): OuterJoinInfiniteStreamLikeQuery[ReqlJoinResult[T, U]] = new OuterJoinInfiniteStreamLikeQuery[ReqlJoinResult[T, U]] {
      val command = TermType.OUTER_JOIN
      val string = "outer_join"
      val arguments = infiniteStreamLike :: otherSeq :: Func.wrap2(predicate) :: Nil
      val options = Options.empty
    }
  }

  implicit class OuterJoinOnFiniteStreamLikeOp[T <: ReqlDatum : Transmuter](val finiteStreamLike: ReqlFiniteStreamLike[T]) {
    def outerJoin[
      U <: ReqlDatum : Transmuter
    ](
      otherSeq: ReqlFiniteSequence[U],
      predicate: (T, U) => ReqlBoolean
    ): OuterJoinFiniteStreamLikeQuery[ReqlJoinResult[T, U]] = new OuterJoinFiniteStreamLikeQuery[ReqlJoinResult[T, U]] {
      val command = TermType.OUTER_JOIN
      val string = "outer_join"
      val arguments = finiteStreamLike :: otherSeq :: Func.wrap2(predicate) :: Nil
      val options = Options.empty
    }
  }

  implicit class OuterJoinOnArrayLikeOp[T <: ReqlDatum : Transmuter](val arrayLike: ReqlFiniteArrayLike[T]) {
    def outerJoin[
      U <: ReqlDatum : Transmuter
    ](
      otherSeq: ReqlFiniteSequence[U],
      predicate: (T, U) => ReqlBoolean
    ): OuterJoinArrayLikeQuery[ReqlJoinResult[T, U]] = new OuterJoinArrayLikeQuery[ReqlJoinResult[T, U]] {
      val command = TermType.OUTER_JOIN
      val string = "outer_join"
      val arguments = arrayLike :: otherSeq :: Func.wrap2(predicate) :: Nil
      val options = Options.empty
    }
  }

  // eq_join
  trait EqJoinInfiniteStreamLikeQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait EqJoinFiniteStreamLikeQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait EqJoinArrayLikeQuery[T <: ReqlDatum] extends ReqlArray[T]

  //TODO: maybe make index argument explicit and remove selector wrapper ??? pass default index is easy, find wrapped - hard

  implicit class EqJoinOnInfiniteStreamLikeOp[T0 <: ReqlDatum](val infiniteStreamLike: ReqlInfiniteStreamLike[T0]) {
    def eqJoin[T1 <: ReqlObject, Selected <: ReqlDatum](
      selector: ReqlDatumSelector[T0, Selected],
      secondTable: ReqlTable[T1],
      secondTableIndex: IndexOptions = DefaultIndex,
      ordered: OrderingOptions = NotOrdered
    ): EqJoinInfiniteStreamLikeQuery[ReqlJoinResult[T0, T1]] = new EqJoinInfiniteStreamLikeQuery[ReqlJoinResult[T0, T1]] {
      val command = TermType.EQ_JOIN
      val string = "eq_join"
      val arguments = infiniteStreamLike :: selector :: secondTable :: Nil
      val options = ComposableOptions.compose(secondTableIndex, ordered)
    }
  }

  implicit class EqJoinOnFiniteStreamLikeOp[T0 <: ReqlDatum](val finiteStreamLike: ReqlFiniteStreamLike[T0]) {
    def eqJoin[T1 <: ReqlObject, Selected <: ReqlDatum](
      selector: ReqlDatumSelector[T0, Selected],
      secondTable: ReqlTable[T1],
      secondTableIndex: IndexOptions = DefaultIndex,
      ordered: OrderingOptions = NotOrdered
    ): EqJoinFiniteStreamLikeQuery[ReqlJoinResult[T0, T1]] = new EqJoinFiniteStreamLikeQuery[ReqlJoinResult[T0, T1]] {
      val command = TermType.EQ_JOIN
      val string = "eq_join"
      val arguments = finiteStreamLike :: selector :: secondTable :: Nil
      val options = ComposableOptions.compose(secondTableIndex, ordered)
    }
  }

  implicit class EqJoinOnArrayLikeOp[T0 <: ReqlDatum](val arrayLike: ReqlFiniteArrayLike[T0]) {
    def eqJoin[T1 <: ReqlObject, Selected <: ReqlDatum](
      selector: ReqlDatumSelector[T0, Selected],
      table: ReqlTable[T1],
      tableIndex: IndexOptions = DefaultIndex,
      ordered: OrderingOptions = NotOrdered
    ): EqJoinArrayLikeQuery[ReqlJoinResult[T0, T1]] = new EqJoinArrayLikeQuery[ReqlJoinResult[T0, T1]] {
      val command = TermType.EQ_JOIN
      val string = "eq_join"
      val arguments = arrayLike :: selector :: table :: Nil
      val options = ComposableOptions.compose(tableIndex, ordered)
    }
  }

  // zip
  //TODO: maybe need allow zip only join results, not on generic stream and array
  // TODO: Documentation says what zip works only on join results - stream and array. In practice it can work on any
  // sequence if all elements have both "left" and "right" fields.
  trait ZipInfiniteStreamQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait ZipFiniteStreamQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]
  trait ZipArrayQuery[T <: ReqlDatum] extends ReqlArray[T]

  //TODO: special input type LeftRight[T, U]
  implicit class ZipOnInfiniteStreamOp[
    LeftType <: ReqlDatum, RightType <: ReqlDatum, Zipped <: ReqlDatum
  ](val infiniteStream: ReqlInfiniteStream[ReqlJoinResult[LeftType, RightType]]) {
    def zip(): ZipInfiniteStreamQuery[Zipped] = new ZipInfiniteStreamQuery[Zipped] {
      val command = TermType.ZIP
      val string = "zip"
      val arguments = infiniteStream :: Nil
      val options = Options.empty
    }
  }

  implicit class ZipOnFiniteStreamOp[
    LeftType <: ReqlDatum, RightType <: ReqlDatum, Zipped <: ReqlDatum
  ](val finiteStream: ReqlFiniteStream[ReqlJoinResult[LeftType, RightType]]) {
    def zip(): ZipFiniteStreamQuery[Zipped] = new ZipFiniteStreamQuery[Zipped] {
      val command = TermType.ZIP
      val string = "zip"
      val arguments = finiteStream :: Nil
      val options = Options.empty
    }
  }

  implicit class ZipOnArrayOp[
    LeftType <: ReqlDatum, RightType <: ReqlDatum, Zipped <: ReqlDatum
  ](val array: ReqlArray[ReqlJoinResult[LeftType, RightType]]) {
    def zip(): ZipArrayQuery[Zipped] = new ZipArrayQuery[Zipped] {
      val command = TermType.ZIP
      val string = "zip"
      val arguments = array :: Nil
      val options = Options.empty
    }
  }

}
