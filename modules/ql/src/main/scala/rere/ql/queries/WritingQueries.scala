package rere.ql.queries

import rere.ql.options.all._
import rere.ql.options.{ComposableOptions, Options}
import rere.ql.ql2.Term.TermType
import rere.ql.typeclasses.ObjectProducer
import rere.ql.types._

trait WritingQueries {

  // insert
  trait InsertTableQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]

  implicit class InsertOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    def insert(obj: ReqlModel[T, PK],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               conflict: ConflictOptions[T, PK] = ErrorOnConflict[T, PK]()): InsertTableQuery[T, PK] = new InsertTableQuery[T, PK] {
      val command = TermType.INSERT
      val string = "insert"
      val arguments = table :: obj :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, conflict)
    }

    def insertMany(objects: ReqlArray[ReqlModel[T, PK]],
                   durability: DurabilityOptions = DefaultDurability,
                   returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                   conflict: ConflictOptions[T, PK] = ErrorOnConflict[T, PK]()): InsertTableQuery[T, PK] = new InsertTableQuery[T, PK] {
      val command = TermType.INSERT
      val string = "insert"
      val arguments = table :: objects :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, conflict)
    }
  }

  //TODO: not by spec
  /**
    * Insert new element with auto-generated id. Primary key fields will not be sent to the database.
    */
  implicit class InsertAutoOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    private implicit def modelShape = table.shape

    def insertAuto(
      obj: T,
      durability: DurabilityOptions = DefaultDurability,
      returnChanges: ReturnChangesOptions = DoNotReturnChanges,
      conflict: ConflictOptions[T, PK] = ErrorOnConflict[T, PK]()
    ): InsertTableQuery[T, PK] = new InsertTableQuery[T, PK] {
      val command = TermType.INSERT
      val string = "insert"
      val arguments = table :: modelShape.toReqlUnidentifiableObject(obj) :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, conflict)
    }
  }

  // update
  trait UpdateTableQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait UpdateTableSliceQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait UpdateSelectionOfStreamQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait UpdateSelectionOfArrayQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait UpdateSelectionOfObjectQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]

  implicit class UpdateOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    def update(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateTableQuery[T, PK] = new UpdateTableQuery[T, PK] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = table :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class UpdateOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    def update(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateTableSliceQuery[T, PK] = new UpdateTableSliceQuery[T, PK] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = tableSlice :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class UpdateOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    def update(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateSelectionOfStreamQuery[T, PK] = new UpdateSelectionOfStreamQuery[T, PK] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class UpdateOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    def update(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateSelectionOfArrayQuery[T, PK] = new UpdateSelectionOfArrayQuery[T, PK] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class UpdateOnSelectionOfObjectOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfObject[T, PK]) {
    def update(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
               durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges,
               nonAtomic: NonAtomicOptions = AtomicUpdate): UpdateSelectionOfObjectQuery[T, PK] = new UpdateSelectionOfObjectQuery[T, PK] {
      val command = TermType.UPDATE
      val string = "update"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  // replace
  trait ReplaceTableQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait ReplaceTableSliceQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait ReplaceSelectionOfStreamQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait ReplaceSelectionOfArrayQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait ReplaceSelectionOfObjectQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]

  implicit class ReplaceOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    def replace(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceTableQuery[T, PK] = new ReplaceTableQuery[T, PK] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = table :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class ReplaceOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    def replace(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceTableSliceQuery[T, PK] = new ReplaceTableSliceQuery[T, PK] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = tableSlice :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class ReplaceOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    def replace(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceSelectionOfStreamQuery[T, PK] = new ReplaceSelectionOfStreamQuery[T, PK] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class ReplaceOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    def replace(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceSelectionOfArrayQuery[T, PK] = new ReplaceSelectionOfArrayQuery[T, PK] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  implicit class ReplaceOnSelectionOfObjectOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfObject[T, PK]) {
    def replace(producer: ObjectProducer[ReqlModel[T, PK], ReqlModel[T, PK]],
                durability: DurabilityOptions = DefaultDurability,
                returnChanges: ReturnChangesOptions = DoNotReturnChanges,
                nonAtomic: NonAtomicOptions = AtomicUpdate): ReplaceSelectionOfObjectQuery[T, PK] = new ReplaceSelectionOfObjectQuery[T, PK] {
      val command = TermType.REPLACE
      val string = "replace"
      val arguments = sel :: producer :: Nil
      val options = ComposableOptions.compose(durability, returnChanges, nonAtomic)
    }
  }

  // delete
  trait DeleteTableQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait DeleteTableSliceQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait DeleteSelectionOfStreamQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait DeleteSelectionOfArrayQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]
  trait DeleteSelectionOfObjectQuery[T, PK <: PrimaryKey] extends ReqlModificationResult[T, PK]

  implicit class DeleteOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteTableQuery[T, PK] = new DeleteTableQuery[T, PK] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  implicit class DeleteOnTableSliceOp[T, PK <: PrimaryKey](val tableSlice: ReqlTableSlice[T, PK]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteTableSliceQuery[T, PK] = new DeleteTableSliceQuery[T, PK] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = tableSlice :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  implicit class DeleteOnSelectionOfStreamOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfStream[T, PK]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteSelectionOfStreamQuery[T, PK] = new DeleteSelectionOfStreamQuery[T, PK] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = sel :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  implicit class DeleteOnSelectionOfArrayOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfArray[T, PK]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteSelectionOfArrayQuery[T, PK] = new DeleteSelectionOfArrayQuery[T, PK] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = sel :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  implicit class DeleteOnSelectionOfObjectOp[T, PK <: PrimaryKey](val sel: ReqlSelectionOfObject[T, PK]) {
    def delete(durability: DurabilityOptions = DefaultDurability,
               returnChanges: ReturnChangesOptions = DoNotReturnChanges): DeleteSelectionOfObjectQuery[T, PK] = new DeleteSelectionOfObjectQuery[T, PK] {
      val command = TermType.DELETE
      val string = "delete"
      val arguments = sel :: Nil
      val options = ComposableOptions.compose(durability, returnChanges)
    }
  }

  // sync
  trait SyncTableQuery extends ReqlSyncingResult

  implicit class SyncOnTableOp[T, PK <: PrimaryKey](val table: ReqlTable[T, PK]) {
    def sync(): SyncTableQuery = new SyncTableQuery {
      val command = TermType.SYNC
      val string = "sync"
      val arguments = table :: Nil
      val options = Options.empty
    }
  }

}
