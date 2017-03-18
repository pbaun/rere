package rere.ql.queries

import rere.ql.options.{ComposableOptions, Options}
import rere.ql.options.all._
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait WritingQueries {

  // insert
  trait InsertTableQuery[T <: ReqlObject] extends ReqlModificationResult[T]

  implicit class InsertOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def insert(obj: T,
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               conflict: ConflictOptions[T] = ErrorOnConflict): InsertTableQuery[T] = new InsertTableQuery[T] {
      val command = TermType.INSERT
      val string = "insert"
      val arguments = table :: obj :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, conflict)
    }

    def insertMany(objects: ReqlArray[T],
                   durability: DurabilityOptions = DefaultDurability,
                   returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                   conflict: ConflictOptions[T] = ErrorOnConflict): InsertTableQuery[T] = new InsertTableQuery[T] {
      val command = TermType.INSERT
      val string = "insert"
      val arguments = table :: objects :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, conflict)
    }
  }

  // update
  trait UpdateTableQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait UpdateTableSliceQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait UpdateSelectionOfStreamQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait UpdateSelectionOfArrayQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait UpdateSelectionOfObjectQuery[T <: ReqlObject] extends ReqlModificationResult[T]

  implicit class UpdateOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def update(producer: ReqlObjectProducer[T, T],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateTableQuery[T] = new UpdateTableQuery[T] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = table :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class UpdateOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def update(producer: ReqlObjectProducer[T, T],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateTableSliceQuery[T] = new UpdateTableSliceQuery[T] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = tableSlice :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class UpdateOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def update(producer: ReqlObjectProducer[T, T],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateSelectionOfStreamQuery[T] = new UpdateSelectionOfStreamQuery[T] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class UpdateOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def update(producer: ReqlObjectProducer[T, T],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateSelectionOfArrayQuery[T] = new UpdateSelectionOfArrayQuery[T] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class UpdateOnSelectionOfObjectOp[T <: ReqlObject](val sel: ReqlSelectionOfObject[T]) {
    def update(producer: ReqlObjectProducer[T, T],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateSelectionOfObjectQuery[T] = new UpdateSelectionOfObjectQuery[T] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  // replace
  trait ReplaceTableQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait ReplaceTableSliceQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait ReplaceSelectionOfStreamQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait ReplaceSelectionOfArrayQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait ReplaceSelectionOfObjectQuery[T <: ReqlObject] extends ReqlModificationResult[T]

  implicit class ReplaceOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def replace(producer: ReqlObjectProducer[T, T],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceTableQuery[T] = new ReplaceTableQuery[T] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = table :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class ReplaceOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def replace(producer: ReqlObjectProducer[T, T],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceTableSliceQuery[T] = new ReplaceTableSliceQuery[T] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = tableSlice :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class ReplaceOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def replace(producer: ReqlObjectProducer[T, T],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceSelectionOfStreamQuery[T] = new ReplaceSelectionOfStreamQuery[T] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class ReplaceOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def replace(producer: ReqlObjectProducer[T, T],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceSelectionOfArrayQuery[T] = new ReplaceSelectionOfArrayQuery[T] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class ReplaceOnSelectionOfObjectOp[T <: ReqlObject](val sel: ReqlSelectionOfObject[T]) {
    def replace(producer: ReqlObjectProducer[T, T],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceSelectionOfObjectQuery[T] = new ReplaceSelectionOfObjectQuery[T] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  // delete
  trait DeleteTableQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait DeleteTableSliceQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait DeleteSelectionOfStreamQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait DeleteSelectionOfArrayQuery[T <: ReqlObject] extends ReqlModificationResult[T]
  trait DeleteSelectionOfObjectQuery[T <: ReqlObject] extends ReqlModificationResult[T]

  implicit class DeleteOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteTableQuery[T] = new DeleteTableQuery[T] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  implicit class DeleteOnTableSliceOp[T <: ReqlObject](val tableSlice: ReqlTableSlice[T]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteTableSliceQuery[T] = new DeleteTableSliceQuery[T] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = tableSlice :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  implicit class DeleteOnSelectionOfStreamOp[T <: ReqlObject](val sel: ReqlSelectionOfStream[T]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteSelectionOfStreamQuery[T] = new DeleteSelectionOfStreamQuery[T] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = sel :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  implicit class DeleteOnSelectionOfArrayOp[T <: ReqlObject](val sel: ReqlSelectionOfArray[T]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteSelectionOfArrayQuery[T] = new DeleteSelectionOfArrayQuery[T] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = sel :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  implicit class DeleteOnSelectionOfObjectOp[T <: ReqlObject](val sel: ReqlSelectionOfObject[T]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteSelectionOfObjectQuery[T] = new DeleteSelectionOfObjectQuery[T] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = sel :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  // sync
  trait SyncTableQuery extends ReqlSyncingResult

  implicit class SyncOnTableOp[T <: ReqlObject](val table: ReqlTable[T]) {
    def sync(): SyncTableQuery = new SyncTableQuery {
      val command = TermType.SYNC
      val string = "sync"
      val arguments = table :: Nil
      val options = Options.empty
    }
  }

}
