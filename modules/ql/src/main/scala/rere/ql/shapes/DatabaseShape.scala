package rere.ql.shapes

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.queries.database.DatabaseQuery
import rere.ql.queries.values
import rere.ql.types.{ReqlDatabase, ReqlTable}

abstract class DatabaseShape(private val databaseName: String) {

  //TODO: allow to preconfigure table (readMode, identifierFormat)
  protected def table[ScalaType, PK](
    tableName: String,
    modelShape: ModelShape[ScalaType, PK]
  ): TableDescriptor[ScalaType, PK] = {
    new TableDescriptor(databaseName, tableName, modelShape)
  }
}

object DatabaseShape {
  implicit class DatabaseShapeOps(val databaseShape: DatabaseShape) extends AnyVal {
    def database(): ReqlDatabase = new DatabaseQuery(values.expr(databaseShape.databaseName))
  }
}

class TableDescriptor[ScalaType, PK](
    databaseName: String,
    tableName: String,
    modelShape: ModelShape[ScalaType, PK]
  ) {

  def shape: ModelShape[ScalaType, PK] = modelShape

  //TODO: support .table options
  def table(): ReqlTable[ScalaType, PK] = {
    val database = new DatabaseQuery(values.expr(databaseName))

    new ReqlTable[ScalaType, PK] {
      def command = TermType.TABLE
      def string = "table"
      def arguments = database :: values.expr(tableName) :: Nil
      def options = Options.empty
      def shape = modelShape
    }
  }
}
