package rere.ql.typeclasses

import rere.ql.types._

trait SequenceUnion[+T <: ReqlDatum, FirstSeq <: ReqlSequence[T]] {
  type OtherSeq <: ReqlSequence[T]
  type Result <: ReqlSequence[T]
}

object SequenceUnion extends LowPrioritySequenceUnion {

  type Aux[+T <: ReqlDatum, First <: ReqlSequence[T], Other <: ReqlSequence[T], Res <: ReqlSequence[T]] = SequenceUnion[T, First] {
    type OtherSeq = Other
    type Result = Res
  }

  /** Array */
  // Array + FiniteArrayLike = Array
  implicit def arrayAndFiniteArrayLike[T <: ReqlDatum, Other <: ReqlFiniteArrayLike[T]]: Aux[T, ReqlArray[T], Other, ReqlArray[T]] = {
    new SequenceUnion[T, ReqlArray[T]] {
      override type OtherSeq = Other
      override type Result = ReqlArray[T]
    }
  }

  /** ReqlSelectionOfArray */
  // FiniteArrayLike + FiniteArrayLike = Array
  implicit def selectionOfArrayAndFiniteArrayLike[
    T,
    PK <: PrimaryKey,
    Other <: ReqlFiniteArrayLike[ReqlModel[T, PK]]
  ]: Aux[ReqlModel[T, PK], ReqlSelectionOfArray[T, PK], Other, ReqlArray[ReqlModel[T, PK]]] = {
    new SequenceUnion[ReqlModel[T, PK], ReqlSelectionOfArray[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlArray[ReqlModel[T, PK]]
    }
  }

  /** Table */
  // FiniteSequence + FiniteSequence = ReqlFiniteStream
  implicit def tableAndFiniteSequence[
    T,
    PK <: PrimaryKey,
    Other <: ReqlFiniteSequence[ReqlModel[T, PK]]
  ]: Aux[ReqlModel[T, PK], ReqlTable[T, PK], Other, ReqlFiniteStream[ReqlModel[T, PK]]] = {
    new SequenceUnion[ReqlModel[T, PK], ReqlTable[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[ReqlModel[T, PK]]
    }
  }

  /** TableSlice */
  // FiniteSequence + FiniteSequence = ReqlFiniteStream
  implicit def tableSliceAndFiniteSequence[
    T,
    PK <: PrimaryKey,
    Other <: ReqlFiniteSequence[ReqlModel[T, PK]]
  ]: Aux[ReqlModel[T, PK], ReqlTableSlice[T, PK], Other, ReqlFiniteStream[ReqlModel[T, PK]]] = {
    new SequenceUnion[ReqlModel[T, PK], ReqlTableSlice[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[ReqlModel[T, PK]]
    }
  }

  /** SelectionOfStream */
  // FiniteSequence + FiniteSequence = ReqlFiniteStream
  implicit def selectionOfStreamAndFiniteSequence[
    T,
    PK <: PrimaryKey,
    Other <: ReqlFiniteSequence[ReqlModel[T, PK]]
  ]: Aux[ReqlModel[T, PK], ReqlSelectionOfStream[T, PK], Other, ReqlFiniteStream[ReqlModel[T, PK]]] = {
    new SequenceUnion[ReqlModel[T, PK], ReqlSelectionOfStream[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[ReqlModel[T, PK]]
    }
  }

  /** SelectionOfStream */
  // FiniteSequence + FiniteSequence = ReqlFiniteStream
  implicit def finiteStreamAndFiniteSequence[
    T <: ReqlDatum,
    Other <: ReqlFiniteSequence[T]
  ]: Aux[T, ReqlFiniteStream[T], Other, ReqlFiniteStream[T]] = {
    new SequenceUnion[T, ReqlFiniteStream[T]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[T]
    }
  }

  /** SelectionOfStream */
  // InfiniteSequence + Sequence = ReqlInfiniteStream
  implicit def infiniteStreamAndSequence[
    T <: ReqlDatum,
    Other <: ReqlSequence[T]
  ]: Aux[T, ReqlInfiniteStream[T], Other, ReqlInfiniteStream[T]] = {
    new SequenceUnion[T, ReqlInfiniteStream[T]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[T]
    }
  }
}

trait LowPrioritySequenceUnion extends EvenMoreLowPrioritySequenceUnion {

  /** Array */
  // Array + FiniteSequence = ReqlFiniteStream
  implicit def arrayAndFiniteSequence[
    T <: ReqlDatum,
    Other <: ReqlFiniteSequence[T]
  ]: SequenceUnion.Aux[T, ReqlArray[T], Other, ReqlFiniteStream[T]] = {
    new SequenceUnion[T, ReqlArray[T]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[T]
    }
  }

  /** ReqlSelectionOfArray */
  // FiniteArrayLike + FiniteSequence = ReqlFiniteStream
  implicit def selectionOfArrayAndFiniteSequence[
    T,
    PK <: PrimaryKey,
    Other <: ReqlFiniteSequence[ReqlModel[T, PK]]
  ]: SequenceUnion.Aux[ReqlModel[T, PK], ReqlSelectionOfArray[T, PK], Other, ReqlFiniteStream[ReqlModel[T, PK]]] = {
    new SequenceUnion[ReqlModel[T, PK], ReqlSelectionOfArray[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[ReqlModel[T, PK]]
    }
  }

  /** Table */
  // FiniteSequence + Sequence = ReqlInfiniteStream
  implicit def tableAndSequence[
    T,
    PK <: PrimaryKey,
    Other <: ReqlSequence[ReqlModel[T, PK]]
  ]: SequenceUnion.Aux[ReqlModel[T, PK], ReqlTable[T, PK], Other, ReqlInfiniteStream[ReqlModel[T, PK]]] = {
    new SequenceUnion[ReqlModel[T, PK], ReqlTable[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[ReqlModel[T, PK]]
    }
  }

  /** TableSlice */
  // FiniteSequence + Sequence = ReqlInfiniteStream
  implicit def tableSliceAndSequence[
    T,
    PK <: PrimaryKey,
    Other <: ReqlSequence[ReqlModel[T, PK]]
  ]: SequenceUnion.Aux[ReqlModel[T, PK], ReqlTableSlice[T, PK], Other, ReqlInfiniteStream[ReqlModel[T, PK]]] = {
    new SequenceUnion[ReqlModel[T, PK], ReqlTableSlice[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[ReqlModel[T, PK]]
    }
  }

  /** SelectionOfStream */
  // FiniteSequence + Sequence = ReqlInfiniteStream
  implicit def selectionOfStreamAndSequence[
    T,
    PK <: PrimaryKey,
    Other <: ReqlSequence[ReqlModel[T, PK]]
  ]: SequenceUnion.Aux[ReqlModel[T, PK], ReqlSelectionOfStream[T, PK], Other, ReqlInfiniteStream[ReqlModel[T, PK]]] = {
    new SequenceUnion[ReqlModel[T, PK], ReqlSelectionOfStream[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[ReqlModel[T, PK]]
    }
  }

  /** FiniteStream */
  // FiniteSequence + Sequence = ReqlInfiniteStream
  implicit def finiteStreamAndSequence[
    T <: ReqlDatum,
    Other <: ReqlSequence[T]
  ]: SequenceUnion.Aux[T, ReqlFiniteStream[T], Other, ReqlInfiniteStream[T]] = {
    new SequenceUnion[T, ReqlFiniteStream[T]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[T]
    }
  }
}

trait EvenMoreLowPrioritySequenceUnion {

  /** Array */
  // Array + Sequence = ReqlInfiniteStream
  implicit def arrayAndSequence[
    T <: ReqlDatum,
    Other <: ReqlSequence[T]
  ]: SequenceUnion.Aux[T, ReqlArray[T], Other, ReqlInfiniteStream[T]] = {
    new SequenceUnion[T, ReqlArray[T]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[T]
    }
  }

  /** ReqlSelectionOfArray */
  // FiniteArrayLike + Sequence = ReqlInfiniteStream
  implicit def selectionOfArrayAndSequence[
    T,
    PK <: PrimaryKey,
    Other <: ReqlSequence[ReqlModel[T, PK]]
  ]: SequenceUnion.Aux[ReqlModel[T, PK], ReqlSelectionOfArray[T, PK], Other, ReqlInfiniteStream[ReqlModel[T, PK]]] = {
    new SequenceUnion[ReqlModel[T, PK], ReqlSelectionOfArray[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[ReqlModel[T, PK]]
    }
  }

}
