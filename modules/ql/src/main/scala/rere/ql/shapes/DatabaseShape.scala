package rere.ql.shapes

import rere.ql.options.ComposableOptions
import rere.ql.options.all._
import rere.ql.ql2.Term.TermType
import rere.ql.queries.db.DatabaseQuery
import rere.ql.queries.values
import rere.ql.types.{ReqlDatabase, ReqlShapable, ReqlString, ReqlTable}

abstract class DatabaseShape(private val databaseName: String) {
  protected def table[ScalaType, PK](
    tableName: String,
    modelShape: ModelShape[ScalaType, PK],
    readMode: ReadModeOptions = DefaultReadMode,
    identifierFormat: IdentifierFormatOptions = DefaultIdentifierFormat
  ): TableDescriptor[ScalaType, PK] = {
    new TableDescriptor(databaseName, tableName, modelShape, readMode, identifierFormat)
  }
}

object DatabaseShape {
  implicit class DatabaseShapeOps(val databaseShape: DatabaseShape) extends AnyVal {
    def database(): ReqlDatabase = new DatabaseQuery(values.expr(databaseShape.databaseName))
  }
}

private class DatabaseTable[ScalaType, PK](
    database: ReqlDatabase,
    tableName: ReqlString,
    modelShape: ModelShape[ScalaType, PK],
    readMode: ReadModeOptions,
    identifierFormat: IdentifierFormatOptions
  ) extends ReqlTable[ScalaType, PK] {

  def command = TermType.TABLE
  def string = "table"
  def arguments = database :: tableName :: Nil
  def options = ComposableOptions.compose(readMode, identifierFormat)
  def shape = modelShape
}

class TableDescriptor[ScalaType, PK](
    databaseName: String,
    tableName: String,
    modelShape: ModelShape[ScalaType, PK],
    readMode: ReadModeOptions,
    identifierFormat: IdentifierFormatOptions
  ) extends ReqlShapable[ScalaType, PK] {

  override def shape: ModelShape[ScalaType, PK] = modelShape

  def table(): ReqlTable[ScalaType, PK] = {
    val database = new DatabaseQuery(values.expr(databaseName))
    new DatabaseTable(database, values.expr(tableName), modelShape, readMode, identifierFormat)
  }

  def table(
    readMode: ReadModeOptions
  ): ReqlTable[ScalaType, PK] = {
    val database = new DatabaseQuery(values.expr(databaseName))
    new DatabaseTable(database, values.expr(tableName), modelShape, readMode, identifierFormat)
  }

  def table(
    identifierFormat: IdentifierFormatOptions
  ): ReqlTable[ScalaType, PK] = {
    val database = new DatabaseQuery(values.expr(databaseName))
    new DatabaseTable(database, values.expr(tableName), modelShape, readMode, identifierFormat)
  }

  def table(
    readMode: ReadModeOptions,
    identifierFormat: IdentifierFormatOptions
  ): ReqlTable[ScalaType, PK] = {
    val database = new DatabaseQuery(values.expr(databaseName))
    new DatabaseTable(database, values.expr(tableName), modelShape, readMode, identifierFormat)
  }
}
