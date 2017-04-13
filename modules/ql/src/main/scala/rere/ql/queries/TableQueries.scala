package rere.ql.queries

import rere.ql.options.all._
import rere.ql.options.{ComposableOptions, Options}
import rere.ql.ql2.Term.TermType
import rere.ql.shapes.{ModelShape, ReqlModel}
import rere.ql.types._

trait TableQueries {

  // table
  trait TableQuery[T, PK] extends ReqlTable[T, PK]

  implicit class TableOnDbOp(database: ReqlDatabase) {
    def table[T, PK](
      name: ReqlString,
      readMode: ReadModeOptions = DefaultReadMode,
      identifierFormat: IdentifierFormatOptions = DefaultIdentifierFormat
    )(
      implicit modelShape: ModelShape[T, PK]
    ): TableQuery[T, PK] = new TableQuery[T, PK] {
      def command = TermType.TABLE
      def string = "table"
      def arguments = database :: name :: Nil
      def options = ComposableOptions.compose(readMode, identifierFormat)
      def shape = modelShape
    }
  }

  implicit class TableOnROp(r: ReqlR) {
    def table[T, PK](
      name: ReqlString,
      readMode: ReadModeOptions = DefaultReadMode,
      identifierFormat: IdentifierFormatOptions = DefaultIdentifierFormat
    )(
      implicit modelShape: ModelShape[T, PK]
    ): TableQuery[T, PK] = new TableQuery[T, PK] {
      def command = TermType.TABLE
      def string = "table"
      def arguments = name :: Nil
      def options = ComposableOptions.compose(readMode, identifierFormat)
      def shape = modelShape
    }
  }

  // table_create
  trait TableCreateDbQuery extends ReqlTableCreationResult
  trait TableCreateRQuery extends ReqlTableCreationResult

  implicit class TableCreateOnDbOp(db: ReqlDatabase) {
    def tableCreate(
      tableName: ReqlString,
      primaryKey: PrimaryKeyOptions = DefaultPrimaryKey,
      durability: DurabilityOptions = DefaultDurability,
      shards: ShardsOptions = SingleShard,
      replicasOptions: ReplicasOptions = SingleReplica
    ): TableCreateDbQuery = new TableCreateDbQuery {
      val command = TermType.TABLE_CREATE
      val string = "table_create"
      val arguments = db :: tableName :: Nil
      val options = ComposableOptions.compose(primaryKey, durability, shards, replicasOptions)
    }
  }

  implicit class TableCreateOnROp(r: ReqlR) {
    def tableCreate(
      tableName: ReqlString,
      primaryKey: PrimaryKeyOptions = DefaultPrimaryKey,
      durability: DurabilityOptions = DefaultDurability,
      shards: ShardsOptions = SingleShard,
      replicasOptions: ReplicasOptions = SingleReplica
    ): TableCreateRQuery = new TableCreateRQuery {
      val command = TermType.TABLE_CREATE
      val string = "table_create"
      val arguments = tableName :: Nil
      val options = ComposableOptions.compose(primaryKey, durability, shards, replicasOptions)
    }
  }

  // table_drop
  trait TableDropDbQuery extends ReqlTableDroppingResult

  implicit class TableDropOnDbOp(db: ReqlDatabase) {
    def tableDrop(tableName: ReqlString): TableDropDbQuery = new TableDropDbQuery {
      val command = TermType.TABLE_DROP
      val string = "table_drop"
      val arguments = db :: tableName :: Nil
      val options = Options.empty
    }
  }

  // table_list
  trait TableListDbQuery extends ReqlArray[ReqlString]

  implicit class TableListOnDbOp(db: ReqlDatabase) {
    def tableList(): TableListDbQuery = new TableListDbQuery {
      val command = TermType.TABLE_LIST
      val string = "table_list"
      val arguments = db :: Nil
      val options = Options.empty
    }
  }

  // index_create
  trait IndexCreateTableQuery extends ReqlIndexCreationResult

  //TODO: function may be binary field from .indexStatus or .indexWait response
  implicit class IndexCreateOnTableOp[T, PK](table: ReqlTable[T, PK]) {
    private implicit def modelShape = table.shape

    def indexCreate(indexName: ReqlString): IndexCreateTableQuery = new IndexCreateTableQuery {
      val command = TermType.INDEX_CREATE
      val string = "index_create"
      val arguments = table :: indexName :: Nil
      val options = Options.empty
    }

    def indexCreate(indexName: ReqlString,
                    multiplicity: IndexMultiplicityOptions,
                    nature: IndexNatureOptions): IndexCreateTableQuery = new IndexCreateTableQuery {
      val command = TermType.INDEX_CREATE
      val string = "index_create"
      val arguments = table :: indexName :: Nil
      val options = ComposableOptions.compose(multiplicity, nature)
    }

    def indexCreate(indexName: ReqlString,
                    indexFunction: ReqlModel[T, PK] => ReqlDatum): IndexCreateTableQuery = new IndexCreateTableQuery {
      val command = TermType.INDEX_CREATE
      val string = "index_create"
      val arguments = table :: indexName :: Func.wrap1(indexFunction) :: Nil
      val options = Options.empty
    }

    def indexCreate(indexName: ReqlString,
                    indexFunction: ReqlModel[T, PK] => ReqlDatum,
                    multiplicity: IndexMultiplicityOptions,
                    nature: IndexNatureOptions): IndexCreateTableQuery = new IndexCreateTableQuery {
      val command = TermType.INDEX_CREATE
      val string = "index_create"
      val arguments = table :: indexName :: Func.wrap1(indexFunction) :: Nil
      val options = ComposableOptions.compose(multiplicity, nature)
    }
  }

  // index_drop
  trait IndexDropTableQuery extends ReqlIndexDroppingResult

  implicit class IndexDropOnTableOp[T, PK](table: ReqlTable[T, PK]) {
    def indexDrop(indexName: ReqlString): IndexDropTableQuery = new IndexDropTableQuery {
      val command = TermType.INDEX_DROP
      val string = "index_drop"
      val arguments = table :: indexName :: Nil
      val options = Options.empty
    }
  }

  // index_list
  trait IndexListTableQuery extends ReqlArray[ReqlString]

  implicit class IndexListOnTableOp[T, PK](table: ReqlTable[T, PK]) {
    def indexList(): IndexListTableQuery = new IndexListTableQuery {
      val command = TermType.INDEX_LIST
      val string = "index_list"
      val arguments = table :: Nil
      val options = Options.empty
    }
  }

  // index_status
  trait IndexStatusTableQuery extends ReqlArray[ReqlIndexStatusResult]
  //TODO: 'function' field of object can be used in .indexCreate (see TODO comment on .indexCreate implementation)

  implicit class IndexStatusOnTableOp[T, PK](table: ReqlTable[T, PK]) {
    def indexStatus(): IndexStatusTableQuery = new IndexStatusTableQuery {
      val command = TermType.INDEX_STATUS
      val string = "index_status"
      val arguments = table :: Nil
      val options = Options.empty
    }

    def indexStatus(indicesNames: ReqlString*): IndexStatusTableQuery = new IndexStatusTableQuery {
      val command = TermType.INDEX_STATUS
      val string = "index_status"
      val arguments = table :: indicesNames.toList
      val options = Options.empty
    }
  }

  // index_wait
  trait IndexWaitTableQuery extends ReqlArray[ReqlIndexStatusResult]
  //TODO: 'function' field of object can be used in .indexCreate

  implicit class IndexWaitOnTableOp[T, PK](table: ReqlTable[T, PK]) {
    def indexWait(): IndexWaitTableQuery = new IndexWaitTableQuery {
      val command = TermType.INDEX_WAIT
      val string = "index_wait"
      val arguments = table :: Nil
      val options = Options.empty
    }

    def indexWait(indicesNames: ReqlString*): IndexWaitTableQuery = new IndexWaitTableQuery {
      val command = TermType.INDEX_WAIT
      val string = "index_wait"
      val arguments = table :: indicesNames.toList
      val options = Options.empty
    }
  }

  // index_rename
  trait IndexRenameTableQuery extends ReqlIndexRenamingResult
  //TODO: maybe reuse Index type for old and new names?

  implicit class IndexRenameOnTableOp[T, PK](table: ReqlTable[T, PK]) {
    def indexRename(
      oldIndexName: ReqlString,
      newIndexName: ReqlString,
      overwriteOptions: OverwriteOptions = NotOverwrite
    ): IndexRenameTableQuery = new IndexRenameTableQuery {
      val command = TermType.INDEX_RENAME
      val string = "index_rename"
      val arguments = table :: oldIndexName :: newIndexName :: Nil
      val options = overwriteOptions
    }
  }

}
