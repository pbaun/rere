package rere.ql.queries

import rere.ql.options.all._
import rere.ql.options.{ComposableOptions, Options}
import rere.ql.ql2.Term.TermType
import rere.ql.shapes.{ModelShape, ReqlModel}
import rere.ql.typeclasses.{ToUpper, Transmuter}
import rere.ql.types._

trait ControlQueries {

  // args
  //TODO: find all vararg queries (.mul etc)
  trait ArgsQuery extends ReqlArgs

  implicit class ArgsOnROp(val r: ReqlR) {
    def args[T <: ReqlDatum](array: ReqlArray[T]): ArgsQuery = new ArgsQuery {
      val command = TermType.ARGS
      val string = "args"
      val arguments = array :: Nil
      val options = Options.empty
    }
  }

  // do (funcall)
  // from docs: Arguments passed to the do function must be basic data types, and cannot be streams or selections. (Read about ReQL data types.)
  //TODO: make variants with big arity and variant with expression instead function (may be useful to perform several queries)
  trait DoQuery extends ReqlExpr

  implicit class DoOnROp(val r: ReqlR) {
    def do_[
      Out <: ReqlExpr : ToUpper
    ](
      function: () => Out
    ) = {                                               // yep, without explicit type
      ToUpper.apply[Out](
        new DoQuery {
          val command = TermType.FUNCALL
          val string = "do"
          val arguments = Func.wrap0(function) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  implicit class DoOnTOp[T <: ReqlExpr, UpperT <: ReqlExpr](val arg: T)(implicit toUpperT: ToUpper.Aux[T, UpperT]) {
    def do_[
      Out <: ReqlExpr : ToUpper
    ](
      function: UpperT => Out
    ) = {                                               // yep, without explicit type
      ToUpper[Out](
        new DoQuery {
          val command = TermType.FUNCALL
          val string = "do"
          val arguments = Func.wrapDo1(function) :: arg :: Nil
          val options = Options.empty
        }
      )
    }
  }

  // branch
  trait BranchQuery extends ReqlExpr

  //TODO: move dsl to separate extension or make variants with bigger arity?
  //TODO: support of 'truthy' values - everything not (false | null) is true?
  def if_ (condition: ReqlBoolean): EmptyIfOfFullBranchExpression = {
    new EmptyIfOfFullBranchExpression(condition :: Nil)
  }

  class EmptyIfOfFullBranchExpression(reversedArgs: List[ReqlExpr]) {
    def _then_ [Out <: ReqlExpr](thenBranch: Out): IfThenOfFullBranchExpression[Out] = {
      new IfThenOfFullBranchExpression[Out](thenBranch :: reversedArgs)
    }
  }

  class IfThenOfFullBranchExpression[Out <: ReqlExpr](reversedArgs: List[ReqlExpr]) {
    def _else_if_ (nextCondition: ReqlBoolean) = {
      new FilledIfOfFullBranchExpression[Out](nextCondition :: reversedArgs)
    }

    def _else_ (elseBranch: Out): FullBranchExpression[Out] = {
      new FullBranchExpression(elseBranch :: reversedArgs)
    }
  }

  class FilledIfOfFullBranchExpression[Out <: ReqlExpr](reversedArgs: List[ReqlExpr]) {
    def _then_ (thenBranch: Out): IfThenOfFullBranchExpression[Out] = {
      new IfThenOfFullBranchExpression[Out](thenBranch :: reversedArgs)
    }
  }

  class FullBranchExpression[T](reversedArgs: List[ReqlExpr]) {
    def args: List[ReqlExpr] = reversedArgs.reverse
  }

  implicit class BranchOnROp(val r: ReqlR) {
    def branch[
      Out <: ReqlExpr : Transmuter
    ](
      condition: ReqlBoolean,
      thenBranch: Out,
      elseBranch: Out
    ): Out = {
      Transmuter.transmute[Out](
        new BranchQuery {
          val command = TermType.BRANCH
          val string = "branch"
          val arguments = condition :: thenBranch :: elseBranch :: Nil
          val options = Options.empty
        }
      )
    }

    def branch[
      Out <: ReqlExpr : Transmuter
    ](
      dslExpression: FullBranchExpression[Out]
    ): Out = {
      Transmuter.transmute[Out](
        new BranchQuery {
          val command = TermType.BRANCH
          val string = "branch"
          val arguments = dslExpression.args
          val options = Options.empty
        }
      )
    }
  }

  def then_ [Out <: ReqlExpr](thenBranch: Out) = {
    new ThenOfPartialBranchExpression[Out](thenBranch :: Nil)
  }

  class ThenOfPartialBranchExpression[Out <: ReqlExpr](reversedArgs: List[ReqlExpr]) {
    def _else_if_ (nextCondition: ReqlBoolean): FilledIfOfPartialBranchExpression[Out] = {
      new FilledIfOfPartialBranchExpression[Out](nextCondition :: reversedArgs)
    }

    def _else_ (elseBranch: Out): PartialBranchExpression[Out] = {
      new PartialBranchExpression[Out](elseBranch :: reversedArgs)
    }
  }

  class FilledIfOfPartialBranchExpression[Out <: ReqlExpr](reversedArgs: List[ReqlExpr]) {
    def _then_ (thenBranch: Out): ThenOfPartialBranchExpression[Out] = {
      new ThenOfPartialBranchExpression[Out](thenBranch :: reversedArgs)
    }
  }

  class PartialBranchExpression[T](reversedArgs: List[ReqlExpr]) {
    def args: List[ReqlExpr] = reversedArgs.reverse
  }

  implicit class BranchOnBooleanOp(val condition: ReqlBoolean) {
    def branch[
      Out <: ReqlExpr : Transmuter
    ](
      thenBranch: Out,
      elseBranch: Out
    ): Out = {
      Transmuter.transmute[Out](
        new BranchQuery {
          val command = TermType.BRANCH
          val string = "branch"
          val arguments = condition :: thenBranch :: elseBranch :: Nil
          val options = Options.empty
        }
      )
    }

    def branch[
      Out <: ReqlExpr : Transmuter
    ](
      dslExpression: PartialBranchExpression[Out]
    ): Out = {
      Transmuter.transmute[Out](
        new BranchQuery {
          val command = TermType.BRANCH
          val string = "branch"
          val arguments = condition :: dslExpression.args
          val options = Options.empty
        }
      )
    }
  }

  // for_each
  trait ForEachTableQuery[T, PK] extends ReqlModificationResult[T, PK]
  trait ForEachTableSliceQuery[T, PK] extends ReqlModificationResult[T, PK]
  trait ForEachSelectionOfArrayQuery[T, PK] extends ReqlModificationResult[T, PK]
  trait ForEachSelectionOfStreamQuery[T, PK] extends ReqlModificationResult[T, PK]
  trait ForEachFiniteStreamQuery[T, PK] extends ReqlModificationResult[T, PK]
  trait ForEachArrayQuery[T, PK] extends ReqlModificationResult[T, PK]
  //TODO: server implementation allows write_function: ReqlValue => Seq[Write_Op]

  implicit class ForEachOnTableOp[T0, PK0](val table: ReqlTable[T0, PK0])(
    implicit shape: ModelShape[T0, PK0]
  ) {
    def forEach[T1, PK1](
      writeFunction: ReqlModel[T0, PK0] => ReqlModificationResult[T1, PK1]
    ): ForEachTableQuery[T1, PK1] = new ForEachTableQuery[T1, PK1] {
      val command = TermType.FOR_EACH
      val string = "for_each"
      val arguments = table :: Func.wrap1(writeFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ForEachOnTableSliceOp[T0, PK0](val tableSlice: ReqlTableSlice[T0, PK0])(
    implicit shape: ModelShape[T0, PK0]
  ) {
    def forEach[T1, PK1](
      writeFunction: ReqlModel[T0, PK0] => ReqlModificationResult[T1, PK1]
    ): ForEachTableSliceQuery[T1, PK1] = new ForEachTableSliceQuery[T1, PK1] {
      val command = TermType.FOR_EACH
      val string = "for_each"
      val arguments = tableSlice :: Func.wrap1(writeFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ForEachOnSelectionOfArrayOp[T0, PK0](val sel: ReqlSelectionOfArray[T0, PK0])(
    implicit shape: ModelShape[T0, PK0]
  ) {
    def forEach[T1, PK1](
      writeFunction: ReqlModel[T0, PK0] => ReqlModificationResult[T1, PK1]
    ): ForEachSelectionOfArrayQuery[T1, PK1] = new ForEachSelectionOfArrayQuery[T1, PK1] {
      val command = TermType.FOR_EACH
      val string = "for_each"
      val arguments = sel :: Func.wrap1(writeFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ForEachOnSelectionOfStreamOp[T0, PK0](val sel: ReqlSelectionOfStream[T0, PK0])(
    implicit shape: ModelShape[T0, PK0]
  ) {
    def forEach[T1, PK1](
      writeFunction: ReqlModel[T0, PK0] => ReqlModificationResult[T1, PK1]
    ): ForEachSelectionOfStreamQuery[T1, PK1] = new ForEachSelectionOfStreamQuery[T1, PK1] {
      val command = TermType.FOR_EACH
      val string = "for_each"
      val arguments = sel :: Func.wrap1(writeFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ForEachOnFiniteStreamOp[T0 <: ReqlDatum : Transmuter](val finiteStream: ReqlFiniteStream[T0]) {
    def forEach[T1, PK1](
      writeFunction: T0 => ReqlModificationResult[T1, PK1]
    ): ForEachFiniteStreamQuery[T1, PK1] = new ForEachFiniteStreamQuery[T1, PK1] {
      val command = TermType.FOR_EACH
      val string = "for_each"
      val arguments = finiteStream :: Func.wrap1(writeFunction) :: Nil
      val options = Options.empty
    }
  }

  implicit class ForEachOnArrayOp[T0 <: ReqlDatum : Transmuter](val array: ReqlArray[T0]) {
    def forEach[T1, PK1](
      writeFunction: T0 => ReqlModificationResult[T1, PK1]
    ): ForEachArrayQuery[T1, PK1] = new ForEachArrayQuery[T1, PK1] {
      val command = TermType.FOR_EACH
      val string = "for_each"
      val arguments = array :: Func.wrap1(writeFunction) :: Nil
      val options = Options.empty
    }
  }

  // range
  trait RangeInfiniteQuery[T <: ReqlDatum] extends ReqlInfiniteStream[T]
  trait RangeFiniteQuery[T <: ReqlDatum] extends ReqlFiniteStream[T]

  implicit class RangeOnROp(val r: ReqlR) {
    def range(): RangeInfiniteQuery[ReqlInteger] = new RangeInfiniteQuery[ReqlInteger] {
      val command = TermType.RANGE
      val string = "range"
      val arguments = Nil
      val options = Options.empty
    }

    def range(endValue: ReqlInteger): RangeFiniteQuery[ReqlInteger] = new RangeFiniteQuery[ReqlInteger] {
      val command = TermType.RANGE
      val string = "range"
      val arguments = endValue :: Nil
      val options = Options.empty
    }

    def range(startValue: ReqlInteger, endValue: ReqlInteger): RangeFiniteQuery[ReqlInteger] = new RangeFiniteQuery[ReqlInteger] {
      val command = TermType.RANGE
      val string = "range"
      val arguments = startValue :: endValue :: Nil
      val options = Options.empty
    }
  }

  // error
  trait ErrorQuery extends ReqlError

  implicit class ErrorOp(r: ReqlR) {
    def error(message: ReqlString): ErrorQuery = new ErrorQuery {
      def command = TermType.ERROR
      def string = "error"
      def arguments = message :: Nil
      def options = Options.empty
    }
  }

  // default
  //TODO: should work only on nullable type?
  //TODO: by docs it's should work on sequence, but docs not provide examples
  //TODO: it not works on table: r.table("qwerty").default(r.table("abc"))
  //TODO: it can work on .avg, .min, .max
  trait DefaultQuery extends ReqlExpr

  implicit class DefaultOnTOp[T <: ReqlExpr : Transmuter](val arg: T) {
    def default(defaultValue: T): T = {
      Transmuter.transmute[T](
        new DefaultQuery {
          val command = TermType.DEFAULT
          val string = "default"
          val arguments =  arg :: defaultValue :: Nil
          val options = Options.empty
        }
      )
    }

    //TODO: ErrorMessage instead ReqlString
    def default[U](defaultFunction: ReqlString => T): T = {
      Transmuter.transmute[T](
        new DefaultQuery {
          val command = TermType.DEFAULT
          val string = "default"
          val arguments =  arg :: Func.wrap1(defaultFunction) :: Nil
          val options = Options.empty
        }
      )
    }
  }

  // js
  trait JavascriptQuery extends ReqlJS

  implicit class JsOp(val r: ReqlR) {
    def js(
      jsString: ReqlString,
      timeout: JSTimeoutOptions = WithDefaultJSTimeout
    ): JavascriptQuery = new JavascriptQuery {
      val command = TermType.JAVASCRIPT
      val string = "js"
      val arguments = jsString :: Nil
      val options = timeout
    }
  }

  // coerce_to
  class CoerceToQuery(val arguments: List[ReqlExpr]) extends ReqlValue {
    val command = TermType.COERCE_TO
    val string = "coerce_to"
    val options = Options.empty
  }

  trait CoerceSupport[To] {
    def typeName: ReqlString
  }
  case object `float` extends CoerceSupport[ReqlFloat] {
    def typeName: ReqlString = values.expr("number")
  }
  case object `string` extends CoerceSupport[ReqlString] {
    def typeName: ReqlString = values.expr("string")
  }
  case object `array` extends CoerceSupport[ReqlArray[Nothing]] {
    def typeName: ReqlString = values.expr("array")
  }
  case object `object` extends CoerceSupport[ReqlObject] {
    def typeName: ReqlString = values.expr("object")
  }
  case object `binary` extends CoerceSupport[ReqlBinary] {
    def typeName: ReqlString = values.expr("binary")
  }

  implicit class CoerceToOnNullOp(val nullExpr: ReqlNull) {
    def coerceTo(to: `string`.type): ReqlString = {
      Transmuter.transmute[ReqlString](
        new CoerceToQuery(nullExpr :: to.typeName :: Nil)
      )
    }
  }

  implicit class CoerceToOnBooleanOp(val bool: ReqlBoolean) {
    def coerceTo(to: `string`.type): ReqlString = {
      Transmuter.transmute[ReqlString](
        new CoerceToQuery(bool :: to.typeName :: Nil)
      )
    }
  }

  implicit class CoerceToOnNumberOp(val number: ReqlNumber) {
    def coerceTo(to: `string`.type): ReqlString = {
      Transmuter.transmute[ReqlString](
        new CoerceToQuery(number :: to.typeName :: Nil)
      )
    }
  }

  implicit class CoerceToOnStringOp(val str: ReqlString) {
    def coerceTo(to: `float`.type): ReqlFloat = {
      Transmuter.transmute[ReqlFloat](
        new CoerceToQuery(str :: to.typeName :: Nil)
      )
    }

    def coerceTo(to: `string`.type): ReqlString = {
      Transmuter.transmute[ReqlString](
        new CoerceToQuery(str :: to.typeName :: Nil)
      )
    }
    def coerceTo(to: `binary`.type): ReqlBinary = {
      Transmuter.transmute[ReqlBinary](
        new CoerceToQuery(str :: to.typeName :: Nil)
      )
    }
  }

  implicit class CoerceToOnArrayOp[T <: ReqlDatum](val arr: ReqlArray[T]) {
    def coerceTo(to: `string`.type): ReqlString = {
      Transmuter.transmute[ReqlString](
        new CoerceToQuery(arr :: to.typeName :: Nil)
      )
    }
    def coerceTo(to: `array`.type): ReqlArray[T] = {
      Transmuter.transmute[ReqlArray[T]](
        new CoerceToQuery(arr :: to.typeName :: Nil)
      )
    }
    def coerceTo(to: `object`.type): ReqlObject = {
      Transmuter.transmute[ReqlObject](
        new CoerceToQuery(arr :: to.typeName :: Nil)
      )
    }
  }

  implicit class CoerceToOnObjectOp(val obj: ReqlObject) {
    def coerceTo(to: `string`.type): ReqlString = {
      Transmuter.transmute[ReqlString](
        new CoerceToQuery(obj :: to.typeName :: Nil)
      )
    }
    def coerceTo[T <: ReqlDatum](to: `array`.type): ReqlArray[T] = {
      Transmuter.transmute[ReqlArray[T]](
        new CoerceToQuery(obj :: to.typeName :: Nil)
      )
    }
    def coerceTo(to: `object`.type): ReqlObject = {
      Transmuter.transmute[ReqlObject](
        new CoerceToQuery(obj :: to.typeName :: Nil)
      )
    }
  }

  implicit class CoerceToOnBinaryOp(val binary: ReqlBinary) {
    def coerceTo(to: `string`.type): ReqlString = {
      Transmuter.transmute[ReqlString](
        new CoerceToQuery(binary :: to.typeName :: Nil)
      )
    }
  }

  implicit class CoerceToOnSingleSelectionOp[T, PK](val sel: ReqlSelectionOfObject[T, PK]) {
    def coerceTo(to: `string`.type): ReqlString = {
      Transmuter.transmute[ReqlString](
        new CoerceToQuery(sel :: to.typeName :: Nil)
      )
    }
    def coerceTo[FieldsLub <: ReqlDatum](to: `array`.type): ReqlArray[FieldsLub] = {
      Transmuter.transmute[ReqlArray[FieldsLub]](
        new CoerceToQuery(sel :: to.typeName :: Nil)
      )
    }
    def coerceTo(to: `object`.type): ReqlObject = {
      Transmuter.transmute[ReqlObject](
        new CoerceToQuery(sel :: to.typeName :: Nil)
      )
    }
  }

  implicit class CoerceToOnFiniteSequenceOp[T <: ReqlDatum](val seq: ReqlFiniteSequence[T]) {
    def coerceTo(to: `array`.type): ReqlArray[T] = {
      Transmuter.transmute[ReqlArray[T]](
        new CoerceToQuery(seq :: to.typeName :: Nil)
      )
    }
  }

  implicit class CoerceToOnFiniteStreamOp[T <: ReqlDatum](val finiteStream: ReqlFiniteStream[T]) {
    def coerceTo(to: `array`.type): ReqlArray[T] = {
      Transmuter.transmute[ReqlArray[T]](
        new CoerceToQuery(finiteStream :: to.typeName :: Nil)
      )
    }
    def coerceTo(to: `object`.type): ReqlObject = {
      Transmuter.transmute[ReqlObject](
        new CoerceToQuery(finiteStream :: to.typeName :: Nil)
      )
    }
  }

  // type_of
  trait TypeOfAnyQuery extends ReqlString

  implicit class TypeOfOnAnyOp(val any: ReqlExpr) {
    def typeOf(): TypeOfAnyQuery = new TypeOfAnyQuery {
      val command = TermType.TYPE_OF
      val string = "type_of"
      val arguments = any :: Nil
      val options = Options.empty
    }
  }

  // info
  trait InfoQuery extends ReqlObject
  //TODO: different structure of object for each input type
  //TODO: make impossible to call .info after .asc and .desc
  //TODO: make possible both syntax any.info() and r.info(any)

  implicit class InfoOnAnyOp(val any: ReqlExpr) {
    def info(): InfoQuery = new InfoQuery {
      val command = TermType.INFO
      val string = "info"
      val arguments = any :: Nil
      val options = Options.empty
    }
  }

  /*implicit class InfoOnROp(val r: ReqlR) {
    def info(any: ReqlExpr): InfoQuery = new InfoQuery {
      val command = TermType.INFO
      val string = "info"
      val arguments = any :: Nil
      val options = Options.empty
    }
  }*/

  // json
  trait JsonQuery extends ReqlJson

  implicit class JsonOnROp(val r: ReqlR) {
    def json(jsonString: ReqlString): JsonQuery = new JsonQuery {
      val command = TermType.JSON
      val string = "json"
      val arguments = jsonString :: Nil
      val options = Options.empty
    }
  }

  // toJsonString, toJSON
  trait ToJsonStringQuery extends ReqlString

  implicit class ToJsonStringOnTOp(val datum: ReqlDatum) {
    def toJsonString(): ToJsonStringQuery = new ToJsonStringQuery {
      val command = TermType.TO_JSON_STRING
      val string = "to_json_string"
      val arguments = datum :: Nil
      val options = Options.empty
    }

    def toJSON(): ToJsonStringQuery = toJsonString()
  }

  // http
  trait HttpQuery extends ReqlHttpResult

  //TODO: return type inference from resultFormat
  //TODO: check all options and make alternative to case object with execution-time parameters
  //TODO: DSL for query parameters
  //TODO: allow data parameter only with POST, PUT, PATCH, or DELETE methods
  //TODO: make extension points for r.http, something like r.http.form, r.http.upload
  //TODO: when pagination is used it's a stream

  implicit class HttpOp(val r: ReqlR) {
    // Pagination features
    // https://rethinkdb.com/api/javascript/http/
    // https://rethinkdb.com/docs/external-api-access/
    //TODO: separate parameters for Pagination and make [http: String => Stream]

    def http(url: ReqlString,
             timeout: HttpTimeoutOptions = WithDefaultHttpTimeout,
             attempts: HttpAttemptsOptions = WithDefaultHttpAttempts,
             redirects: HttpRedirectsOptions = WithDefaultHttpRedirects,
             verify: HttpVerifyOptions = WithDefaultHttpVerify,
             resultFormat: HttpResultFormatOptions = WithDefaultHttpResultFormat,
             method: HttpMethodOptions = WithDefaultHttpMethod,
             auth: HttpAuthOptions = WithoutHttpAuth,
             params: HttpParamsOptions = WithoutHttpParams,
             header: HttpHeaderOptions = WithDefaultHttpHeader,
             data: HttpDataOptions = WithoutHttpData,
             pagination: HttpPaginationOptions = WithoutHttpPagination): HttpQuery = new HttpQuery {
      val command = TermType.HTTP
      val string = "http"
      val arguments = url :: Nil
      val options = ComposableOptions.compose(
        timeout, attempts, redirects, verify, resultFormat,
        method, auth, params, header, data, pagination)
    }
  }

  // uuid
  trait UuidQuery extends ReqlString

  implicit class UuidOp(val r: ReqlR) {
    def uuid(): UuidQuery = new UuidQuery {
      val command = TermType.UUID
      val string = "uuid"
      val arguments = Nil
      val options = Options.empty
    }

    def uuid(baseString: ReqlString): UuidQuery = new UuidQuery {
      val command = TermType.UUID
      val string = "uuid"
      val arguments = baseString :: Nil
      val options = Options.empty
    }
  }

}
