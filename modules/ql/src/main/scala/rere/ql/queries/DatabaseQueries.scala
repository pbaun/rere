package rere.ql.queries

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait DatabaseQueries {

  // db
  class DatabaseQuery(dbName: ReqlString) extends ReqlDatabase {
    val command = TermType.DB
    val string = "db"
    val arguments = dbName :: Nil
    val options = Options.empty
  }

  implicit class DatabaseOp(val r: ReqlR) {
    def db(dbName: ReqlString): DatabaseQuery = new DatabaseQuery(dbName)
  }

  // db_create
  class DbCreateRQuery(dbName: ReqlString) extends ReqlDatabaseCreationResult {
    val command = TermType.DB_CREATE
    val string = "db_create"
    val arguments = dbName :: Nil
    val options = Options.empty
  }

  implicit class DbCreateOnROp(val r: ReqlR) {
    def dbCreate(dbName: ReqlString): DbCreateRQuery = new DbCreateRQuery(dbName)
  }

  // db_drop
  class DbDropRQuery(dbName: ReqlString) extends ReqlDatabaseDroppingResult {
    val command = TermType.DB_DROP
    val string = "db_drop"
    val arguments = dbName :: Nil
    val options = Options.empty
  }

  implicit class DbDropOnROp(val r: ReqlR) {
    def dbDrop(dbName: ReqlString): DbDropRQuery = new DbDropRQuery(dbName)
  }

  // db_list
  class DbListRQuery extends ReqlArray[ReqlString] {
    val command = TermType.DB_LIST
    val string = "db_list"
    val arguments = Nil
    val options = Options.empty
  }

  implicit class DbListOnROp(val r: ReqlR) {
    def dbList(): DbListRQuery = new DbListRQuery
  }

}
