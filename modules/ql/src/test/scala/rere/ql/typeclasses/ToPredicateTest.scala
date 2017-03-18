package rere.ql.typeclasses

import org.scalatest.FlatSpec
import rere.ql.types._
import rere.ql.queries.values._

class ToPredicateTest extends FlatSpec {

  behavior of "ToPredicate"

  it should "work with values" in {
    ToPredicate(expr(null)): ReqlPredicate[ReqlNull]
    ToPredicate(expr(true)): ReqlPredicate[ReqlBoolean]
    ToPredicate(expr(123)): ReqlPredicate[ReqlInteger]
    ToPredicate(expr(123L)): ReqlPredicate[ReqlInteger]
    ToPredicate(expr(BigInt(123))): ReqlPredicate[ReqlInteger]
    ToPredicate(expr(123.45)): ReqlPredicate[ReqlFloat]
    ToPredicate(expr(BigDecimal(123.45))): ReqlPredicate[ReqlFloat]
    ToPredicate(expr("abc")): ReqlPredicate[ReqlString]
  }

  it should "work with functions" in {
    ToPredicate({_: ReqlBoolean => expr(true)}): ReqlPredicate[ReqlBoolean]
    ToPredicate({_: ReqlInteger => expr(true)}): ReqlPredicate[ReqlInteger]
    ToPredicate({_: ReqlFloat => expr(true)}): ReqlPredicate[ReqlFloat]
    ToPredicate({_: ReqlString => expr(true)}): ReqlPredicate[ReqlString]
  }

  it should "allow to overloading methods" in {
    class ValueContainer[T <: ReqlDatum : Transmuter : ToPredicate](v: T) {
      def testMethod(value: T): ReqlExpr = {
        ToPredicate(value)
      }

      def testMethod(func: T => ReqlBoolean): ReqlExpr = {
        ToPredicate(func)
      }
    }

    val cont = new ValueContainer[ReqlString]("abc")
    cont.testMethod(_ => rere.ql.queries.values.expr(true))
    cont.testMethod(_ => true)
    cont.testMethod("abc")
  }

}
