package rere.ql.shapes

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.queries.database.DatabaseQuery
import rere.ql.queries.values
import rere.ql.types.{ReqlDatabase, ReqlTable}

abstract class DatabaseShape(private val databaseName: String) {

  //TODO: allow to preconfigure table (readMode, identifierFormat)
  protected def table[Model, PK](tableName: String, modelShape: ModelShape[Model, PK]): TableDescriptor[Model, PK] = {
    new TableDescriptor(databaseName, tableName, modelShape)
  }
}

object DatabaseShape {
  implicit class DatabaseShapeOps(val databaseShape: DatabaseShape) extends AnyVal {
    def database(): ReqlDatabase = new DatabaseQuery(values.expr(databaseShape.databaseName))
  }
}

class TableDescriptor[Model, PK](databaseName: String, tableName: String, modelShape: ModelShape[Model, PK]) {
  def shape: ModelShape[Model, PK] = modelShape

  //TODO: support .table options
  def table(): ReqlTable[ReqlModel[Model], PK] = {
    val database = new DatabaseQuery(values.expr(databaseName))

    new ReqlTable[ReqlModel[Model], PK] {
      val command = TermType.TABLE
      val string = "table"
      val arguments = database :: values.expr(tableName) :: Nil
      val options = Options.empty
    }
  }
}
