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
  implicit def selectionOfArrayAndFiniteArrayLike[T <: ReqlObject, PK, Other <: ReqlFiniteArrayLike[T]]: Aux[T, ReqlSelectionOfArray[T, PK], Other, ReqlArray[T]] = {
    new SequenceUnion[T, ReqlSelectionOfArray[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlArray[T]
    }
  }

  /** Table */
  // FiniteSequence + FiniteSequence = ReqlFiniteStream
  implicit def tableAndFiniteSequence[
    T <: ReqlObject,
    PK,
    Other <: ReqlFiniteSequence[T]
  ]: Aux[T, ReqlTable[T, PK], Other, ReqlFiniteStream[T]] = {
    new SequenceUnion[T, ReqlTable[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[T]
    }
  }

  /** TableSlice */
  // FiniteSequence + FiniteSequence = ReqlFiniteStream
  implicit def tableSliceAndFiniteSequence[
    T <: ReqlObject,
    PK,
    Other <: ReqlFiniteSequence[T]
  ]: Aux[T, ReqlTableSlice[T, PK], Other, ReqlFiniteStream[T]] = {
    new SequenceUnion[T, ReqlTableSlice[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[T]
    }
  }

  /** SelectionOfStream */
  // FiniteSequence + FiniteSequence = ReqlFiniteStream
  implicit def selectionOfStreamAndFiniteSequence[
    T <: ReqlObject,
    PK,
    Other <: ReqlFiniteSequence[T]
  ]: Aux[T, ReqlSelectionOfStream[T, PK], Other, ReqlFiniteStream[T]] = {
    new SequenceUnion[T, ReqlSelectionOfStream[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[T]
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
    T <: ReqlObject,
    PK,
    Other <: ReqlFiniteSequence[T]
  ]: SequenceUnion.Aux[T, ReqlSelectionOfArray[T, PK], Other, ReqlFiniteStream[T]] = {
    new SequenceUnion[T, ReqlSelectionOfArray[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlFiniteStream[T]
    }
  }

  /** Table */
  // FiniteSequence + Sequence = ReqlInfiniteStream
  implicit def tableAndSequence[
    T <: ReqlObject,
    PK,
    Other <: ReqlSequence[T]
  ]: SequenceUnion.Aux[T, ReqlTable[T, PK], Other, ReqlInfiniteStream[T]] = {
    new SequenceUnion[T, ReqlTable[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[T]
    }
  }

  /** TableSlice */
  // FiniteSequence + Sequence = ReqlInfiniteStream
  implicit def tableSliceAndSequence[
    T <: ReqlObject,
    PK,
    Other <: ReqlSequence[T]
  ]: SequenceUnion.Aux[T, ReqlTableSlice[T, PK], Other, ReqlInfiniteStream[T]] = {
    new SequenceUnion[T, ReqlTableSlice[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[T]
    }
  }

  /** SelectionOfStream */
  // FiniteSequence + Sequence = ReqlInfiniteStream
  implicit def selectionOfStreamAndSequence[
    T <: ReqlObject,
    PK,
    Other <: ReqlSequence[T]
  ]: SequenceUnion.Aux[T, ReqlSelectionOfStream[T, PK], Other, ReqlInfiniteStream[T]] = {
    new SequenceUnion[T, ReqlSelectionOfStream[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[T]
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
    T <: ReqlObject,
    PK,
    Other <: ReqlSequence[T]
  ]: SequenceUnion.Aux[T, ReqlSelectionOfArray[T, PK], Other, ReqlInfiniteStream[T]] = {
    new SequenceUnion[T, ReqlSelectionOfArray[T, PK]] {
      override type OtherSeq = Other
      override type Result = ReqlInfiniteStream[T]
    }
  }

}
