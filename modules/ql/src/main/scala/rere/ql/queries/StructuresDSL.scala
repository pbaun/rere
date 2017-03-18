package rere.ql.queries

import io.circe.{Json, JsonObject}
import rere.ql.types._
import rere.ql.values.{DSLList, DSLObject}

class DSLKeyValuePair(val key: ReqlString, val datum: ReqlDatum) extends DSLObject {
  override def ~ (anotherPair: DSLKeyValuePair): DSLKeyValuePairList = {
    new DSLKeyValuePairList(anotherPair :: this :: Nil)
  }

  override def pairs: List[DSLKeyValuePair] = this :: Nil
}

class DSLKeyValuePairList(reversedPairs: List[DSLKeyValuePair]) extends DSLObject {
  override def ~ (anotherPair: DSLKeyValuePair): DSLKeyValuePairList = {
    new DSLKeyValuePairList(anotherPair :: reversedPairs)
  }

  override def pairs: List[DSLKeyValuePair] = reversedPairs.reverse
}

trait StructuresDSL {

  implicit class ReqlObjectKeyReqlStringOp(val key: ReqlString) {
    def := (datum: ReqlDatum): DSLKeyValuePair = {
      new DSLKeyValuePair(key, datum)
    }
  }

  implicit class ReqlObjectKeyStringOp(val key: String) {
    def := (datum: ReqlDatum): DSLKeyValuePair = {
      new DSLKeyValuePair(values.expr(key), datum)
    }
  }

  trait ReList[+T <: ReqlDatum] extends DSLList[T]

  case object ReNil extends ReList[Nothing] {
    override def arguments: List[ReqlExpr] = Nil

    def ::(nullRef: Null): ReConsNull = ReConsNull(values.expr(nullRef) :: Nil)
    def ::(bool: Boolean): ReConsBoolean = ReConsBoolean(values.expr(bool) :: Nil)
    def ::(number: Int): ReConsInteger = ReConsInteger(values.expr(number) :: Nil)
    def ::(number: Long): ReConsInteger = ReConsInteger(values.expr(number) :: Nil)
    def ::(number: BigInt): ReConsInteger = ReConsInteger(values.expr(number) :: Nil)
    def ::(number: BigDecimal): ReConsFloat = ReConsFloat(values.expr(number) :: Nil)
    def ::(string: String): ReConsString = ReConsString(values.expr(string) :: Nil)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsArray[T] = ReConsArray[T](array :: Nil)
    def ::(jsonArray: List[Json]): ReConsArray[ReqlJson] = ReConsArray(values.expr(jsonArray) :: Nil)
    def ::(obj: ReqlObject): ReConsObject = ReConsObject(obj :: Nil)
    def ::(jsonObj: JsonObject): ReConsObject = ReConsObject(values.expr(jsonObj) :: Nil)
    def ::(json: Json): ReConsJson = ReConsJson(values.expr(json) :: Nil)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: Nil)
  }

  case class ReConsNull(list: List[ReqlExpr]) extends ReList[ReqlNull] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsNull = ReConsNull(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsDatum = ReConsDatum(values.expr(bool) :: list)
    def ::(number: Int): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: Long): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigInt): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(string: String): ReConsDatum = ReConsDatum(values.expr(string) :: list)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsDatum = ReConsDatum(array :: list)
    def ::(obj: ReqlObject): ReConsDatum = ReConsDatum(obj :: list)
    def ::(json: Json): ReConsDatum = ReConsDatum(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

  case class ReConsBoolean(list: List[ReqlExpr]) extends ReList[ReqlBoolean] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsDatum = ReConsDatum(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsBoolean = ReConsBoolean(values.expr(bool) :: list)
    def ::(number: Int): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: Long): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigInt): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(string: String): ReConsDatum = ReConsDatum(values.expr(string) :: list)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsDatum = ReConsDatum(array :: list)
    def ::(obj: ReqlObject): ReConsDatum = ReConsDatum(obj :: list)
    def ::(json: Json): ReConsDatum = ReConsDatum(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

  case class ReConsInteger(list: List[ReqlExpr]) extends ReList[ReqlInteger] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsDatum = ReConsDatum(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsDatum = ReConsDatum(values.expr(bool) :: list)
    def ::(number: Int): ReConsInteger = ReConsInteger(values.expr(number) :: list)
    def ::(number: Long): ReConsInteger = ReConsInteger(values.expr(number) :: list)
    def ::(number: BigInt): ReConsInteger = ReConsInteger(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsNumber = ReConsNumber(values.expr(number) :: list)
    def ::(string: String): ReConsDatum = ReConsDatum(values.expr(string) :: list)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsDatum = ReConsDatum(array :: list)
    def ::(obj: ReqlObject): ReConsDatum = ReConsDatum(obj :: list)
    def ::(json: Json): ReConsDatum = ReConsDatum(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

  case class ReConsFloat(list: List[ReqlExpr]) extends ReList[ReqlFloat] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsDatum = ReConsDatum(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsDatum = ReConsDatum(values.expr(bool) :: list)
    def ::(number: Int): ReConsNumber = ReConsNumber(values.expr(number) :: list)
    def ::(number: Long): ReConsNumber = ReConsNumber(values.expr(number) :: list)
    def ::(number: BigInt): ReConsNumber = ReConsNumber(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsFloat = ReConsFloat(values.expr(number) :: list)
    def ::(string: String): ReConsDatum = ReConsDatum(values.expr(string) :: list)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsDatum = ReConsDatum(array :: list)
    def ::(obj: ReqlObject): ReConsDatum = ReConsDatum(obj :: list)
    def ::(json: Json): ReConsDatum = ReConsDatum(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

  case class ReConsNumber(list: List[ReqlExpr]) extends ReList[ReqlNumber] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsDatum = ReConsDatum(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsDatum = ReConsDatum(values.expr(bool) :: list)
    def ::(number: Int): ReConsNumber = ReConsNumber(values.expr(number) :: list)
    def ::(number: Long): ReConsNumber = ReConsNumber(values.expr(number) :: list)
    def ::(number: BigInt): ReConsNumber = ReConsNumber(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsNumber = ReConsNumber(values.expr(number) :: list)
    def ::(string: String): ReConsDatum = ReConsDatum(values.expr(string) :: list)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsDatum = ReConsDatum(array :: list)
    def ::(obj: ReqlObject): ReConsDatum = ReConsDatum(obj :: list)
    def ::(json: Json): ReConsDatum = ReConsDatum(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

  case class ReConsString(list: List[ReqlExpr]) extends ReList[ReqlString] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsDatum = ReConsDatum(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsDatum = ReConsDatum(values.expr(bool) :: list)
    def ::(number: Int): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: Long): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigInt): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(string: String): ReConsString = ReConsString(values.expr(string) :: list)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsDatum = ReConsDatum(array :: list)
    def ::(obj: ReqlObject): ReConsDatum = ReConsDatum(obj :: list)
    def ::(json: Json): ReConsDatum = ReConsDatum(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

  case class ReConsArray[T <: ReqlDatum](list: List[ReqlExpr]) extends ReList[ReqlArray[T]] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsDatum = ReConsDatum(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsDatum = ReConsDatum(values.expr(bool) :: list)
    def ::(number: Int): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: Long): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigInt): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(string: String): ReConsDatum = ReConsDatum(values.expr(string) :: list)
    def ::[U >: T <: ReqlDatum](array: ReqlArray[U]): ReConsArray[U] = ReConsArray[U](array :: list)
    def ::(obj: ReqlObject): ReConsDatum = ReConsDatum(obj :: list)
    def ::(json: Json): ReConsDatum = ReConsDatum(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

  case class ReConsObject(list: List[ReqlExpr]) extends ReList[ReqlObject] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsDatum = ReConsDatum(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsDatum = ReConsDatum(values.expr(bool) :: list)
    def ::(number: Int): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: Long): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigInt): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(string: String): ReConsDatum = ReConsDatum(values.expr(string) :: list)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsDatum = ReConsDatum(array :: list)
    def ::(obj: ReqlObject): ReConsObject = ReConsObject(obj :: list)
    def ::(json: Json): ReConsDatum = ReConsDatum(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

  case class ReConsJson(list: List[ReqlExpr]) extends ReList[ReqlJson] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsDatum = ReConsDatum(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsDatum = ReConsDatum(values.expr(bool) :: list)
    def ::(number: Int): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: Long): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigInt): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(string: String): ReConsDatum = ReConsDatum(values.expr(string) :: list)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsDatum = ReConsDatum(array :: list)
    def ::(obj: ReqlObject): ReConsDatum = ReConsDatum(obj :: list)
    def ::(json: Json): ReConsJson = ReConsJson(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

  case class ReConsDatum(list: List[ReqlExpr]) extends ReList[ReqlDatum] {
    override def arguments: List[ReqlExpr] = list

    def ::(nullRef: Null): ReConsDatum = ReConsDatum(values.expr(nullRef) :: list)
    def ::(bool: Boolean): ReConsDatum = ReConsDatum(values.expr(bool) :: list)
    def ::(number: Int): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: Long): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigInt): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(number: BigDecimal): ReConsDatum = ReConsDatum(values.expr(number) :: list)
    def ::(string: String): ReConsDatum = ReConsDatum(values.expr(string) :: list)
    def ::[T <: ReqlDatum](array: ReqlArray[T]): ReConsDatum = ReConsDatum(array :: list)
    def ::(obj: ReqlObject): ReConsDatum = ReConsDatum(obj :: list)
    def ::(json: Json): ReConsDatum = ReConsDatum(values.expr(json) :: list)
    def ::(datum: ReqlDatum): ReConsDatum = ReConsDatum(datum :: list)
  }

}
