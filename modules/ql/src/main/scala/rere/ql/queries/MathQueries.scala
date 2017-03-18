package rere.ql.queries

import rere.ql.options.Options
import rere.ql.options.all._
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait MathQueries {

  //TODO: r.args support
  //TODO: top level for mul, div, mod ???

  // add
  trait AddQuery extends ReqlExpr {
    val command = TermType.ADD
    val string = "+"
    val options = Options.empty
  }

  trait AddIntegerQuery extends AddQuery with ReqlInteger
  trait AddFloatQuery extends AddQuery with ReqlFloat
  trait AddStringQuery extends AddQuery with ReqlString
  trait AddArrayQuery[T <: ReqlDatum] extends AddQuery with ReqlArray[T]
  trait AddTimeQuery extends AddQuery with ReqlTime

  implicit class AddOnROp(r: ReqlR) {
    // empty args not allowed
    def add(augend: ReqlInteger, addends: ReqlInteger*): AddIntegerQuery = new AddIntegerQuery {
      val arguments = augend :: addends.toList
    }

    def add(augend: ReqlNumber, addends: ReqlNumber*): AddFloatQuery = new AddFloatQuery {
      val arguments = augend :: addends.toList
    }

    def add(augend: ReqlString, addends: ReqlString*): AddStringQuery = new AddStringQuery {
      val arguments = augend :: addends.toList
    }

    def add[T <: ReqlDatum](augend: ReqlArray[T], addends: ReqlArray[T]*): AddArrayQuery[T] = new AddArrayQuery[T] {
      val arguments = augend :: addends.toList
    }

    def add(augend: ReqlTime, addends: ReqlNumber*): AddTimeQuery = new AddTimeQuery {
      val arguments = augend :: addends.toList
    }
  }

  implicit class AddOnIntegerOp(val augend: ReqlInteger) {
    // empty allowed
    def add(addends: ReqlInteger*): AddIntegerQuery = new AddIntegerQuery {
      val arguments = augend :: addends.toList
    }

    def add(addends: ReqlNumber*): AddFloatQuery = new AddFloatQuery {
      val arguments = augend :: addends.toList
    }
  }

  implicit class AddOnNumberOp(val augend: ReqlNumber) {
    // empty allowed
    def add(addends: ReqlNumber*): AddFloatQuery = new AddFloatQuery {
      val arguments = augend :: addends.toList
    }
  }

  implicit class AddOnStringOp(val augend: ReqlString) {
    // empty allowed
    def add(addends: ReqlString*): AddStringQuery = new AddStringQuery {
      val arguments = augend :: addends.toList
    }
  }

  implicit class AddOnArrayOp[T <: ReqlDatum](val augend: ReqlArray[T]) {
    // empty allowed
    def add(addends: ReqlArray[T]*): AddArrayQuery[T] = new AddArrayQuery[T] {
      val arguments = augend :: addends.toList
    }
  }

  implicit class AddOnTimeOp(val augend: ReqlTime) {
    // empty allowed
    def add(addends: ReqlNumber*): AddTimeQuery = new AddTimeQuery {
      val arguments = augend :: addends.toList
    }
  }

  // sub
  trait SubQuery extends ReqlExpr {
    val command = TermType.SUB
    val string = "-"
    val options = Options.empty
  }

  trait SubIntegerQuery extends SubQuery with ReqlInteger
  trait SubFloatQuery extends SubQuery with ReqlFloat
  trait SubTimeQuery extends SubQuery with ReqlTime

  implicit class SubOnROp(r: ReqlR) {
    // empty args not allowed
    def sub(minuend: ReqlInteger, subtrahends: ReqlInteger*): SubIntegerQuery = new SubIntegerQuery {
      val arguments = minuend :: subtrahends.toList
    }

    def sub(minuend: ReqlNumber, subtrahends: ReqlNumber*): SubFloatQuery = new SubFloatQuery {
      val arguments = minuend :: subtrahends.toList
    }

    def sub(minuend: ReqlTime, subtrahends: ReqlNumber*): SubTimeQuery = new SubTimeQuery {
      val arguments = minuend :: subtrahends.toList
    }

    def sub(minuend: ReqlTime, subtrahend: ReqlTime): SubFloatQuery = new SubFloatQuery {
      val arguments = minuend :: subtrahend :: Nil
    }
  }

  implicit class SubOnIntegerOp(val minuend: ReqlInteger) {
    // empty allowed
    def sub(subtrahends: ReqlInteger*): SubIntegerQuery = new SubIntegerQuery {
      val arguments = minuend :: subtrahends.toList
    }

    def sub(subtrahends: ReqlNumber*): SubFloatQuery = new SubFloatQuery {
      val arguments = minuend :: subtrahends.toList
    }
  }

  implicit class SubOnNumberOp(val minuend: ReqlNumber) {
    // empty allowed
    def sub(subtrahends: ReqlNumber*): SubFloatQuery = new SubFloatQuery {
      val arguments = minuend :: subtrahends.toList
    }
  }

  implicit class SubOnTimeOp(val minuend: ReqlTime) {
    // empty allowed
    def sub(subtrahends: ReqlNumber*): SubTimeQuery = new SubTimeQuery {
      val arguments = minuend :: subtrahends.toList
    }

    def sub(subtrahend: ReqlTime): SubFloatQuery = new SubFloatQuery {
      val arguments = minuend :: subtrahend :: Nil
    }
  }

  // mul
  trait MulQuery extends ReqlExpr {
    val command = TermType.MUL
    val string = "*"
    val options = Options.empty
  }

  trait MulIntegerQuery extends MulQuery with ReqlInteger
  trait MulFloatQuery extends MulQuery with ReqlFloat
  trait MulArrayQuery[T <: ReqlDatum] extends MulQuery with ReqlArray[T]

  implicit class MulOnIntegerOp(val multiplier: ReqlInteger) {
    // empty allowed
    def mul(multiplicands: ReqlInteger*): MulIntegerQuery = new MulIntegerQuery {
      val arguments = multiplier :: multiplicands.toList
    }

    def mul(multiplicands: ReqlNumber*): MulFloatQuery = new MulFloatQuery {
      val arguments = multiplier :: multiplicands.toList
    }
  }

  implicit class MulOnNumberOp(val multiplier: ReqlNumber) {
    // empty allowed
    def mul(multiplicands: ReqlNumber*): MulFloatQuery = new MulFloatQuery {
      val arguments = multiplier :: multiplicands.toList
    }
  }

  implicit class MulOnArrayOp[T <: ReqlDatum](val multiplier: ReqlArray[T]) {
    // empty allowed
    // Arrays can only be multiplied on whole (non negative) numbers
    def mul(multiplicands: ReqlInteger*): MulArrayQuery[T] = new MulArrayQuery[T] {
      val arguments = multiplier :: multiplicands.toList
    }
  }

  // div
  trait DivQuery extends ReqlExpr {
    val command = TermType.DIV
    val string = "/"
    val options = Options.empty
  }

  trait DivNumberQuery extends DivQuery with ReqlFloat

  implicit class DivOnNumberOp(val dividend: ReqlNumber) {
    // empty allowed
    def div(divisors: ReqlNumber*): DivNumberQuery = new DivNumberQuery {
      val arguments = dividend :: divisors.toList
    }
  }

  // mod
  trait ModQuery extends ReqlExpr {
    val command = TermType.MOD
    val string = "%"
    val options = Options.empty
  }

  trait ModIntegerQuery extends ModQuery with ReqlInteger

  implicit class ModOnNumberOp(val dividend: ReqlInteger) {
    def mod(divisor: ReqlInteger): ModIntegerQuery = new ModIntegerQuery {
      val arguments = dividend :: divisor :: Nil
    }
  }

  //floor
  trait FloorQuery extends ReqlExpr {
    val command = TermType.FLOOR
    val string = "floor"
    val options = Options.empty
  }

  trait FloorNumberQuery extends FloorQuery with ReqlInteger

  implicit class FloorOnROp(r: ReqlR) {
    // empty args not allowed
    def floor(number: ReqlNumber): FloorNumberQuery = new FloorNumberQuery {
      val arguments = number :: Nil
    }
  }

  implicit class FloorOnNumberOp(val number: ReqlNumber) {
    def floor(): FloorNumberQuery = new FloorNumberQuery {
      val arguments = number :: Nil
    }
  }

  //ceil
  trait CeilQuery extends ReqlExpr {
    val command = TermType.CEIL
    val string = "ceil"
    val options = Options.empty
  }

  trait CeilNumberQuery extends CeilQuery with ReqlInteger

  implicit class CeilOnROp(r: ReqlR) {
    // empty args not allowed
    def ceil(number: ReqlNumber): CeilNumberQuery = new CeilNumberQuery {
      val arguments = number :: Nil
    }
  }

  implicit class CeilOnNumberOp(val number: ReqlNumber) {
    def ceil(): CeilNumberQuery = new CeilNumberQuery {
      val arguments = number :: Nil
    }
  }

  //round
  trait RoundQuery extends ReqlExpr {
    val command = TermType.ROUND
    val string = "round"
    val options = Options.empty
  }

  trait RoundNumberQuery extends RoundQuery with ReqlInteger

  implicit class RoundOnROp(r: ReqlR) {
    // empty args not allowed
    def round(number: ReqlNumber): RoundNumberQuery = new RoundNumberQuery {
      val arguments = number :: Nil
    }
  }

  implicit class RoundOnNumberOp(val number: ReqlNumber) {
    def round(): RoundNumberQuery = new RoundNumberQuery {
      val arguments = number :: Nil
    }
  }

  // random
  trait RandomQuery extends ReqlExpr {
    val command = TermType.RANDOM
    val string = "random"
  }

  trait RandomIntegerOnRQuery extends RandomQuery with ReqlInteger
  trait RandomFloatOnRQuery extends RandomQuery with ReqlFloat

  implicit class RandomOnROp(r: ReqlR) {
    def random(): RandomFloatOnRQuery = new RandomFloatOnRQuery {
      val arguments = Nil
      val options = Options.empty
    }

    def random(upperBound: ReqlInteger): RandomIntegerOnRQuery = new RandomIntegerOnRQuery {
      val arguments = upperBound :: Nil
      val options = Options.empty
    }

    def random(upperBound: ReqlInteger,
               randomOptions: IntegerValues.type): RandomIntegerOnRQuery = new RandomIntegerOnRQuery {
      val arguments = upperBound :: Nil
      val options = randomOptions
    }

    def random(upperBound: ReqlNumber,
               randomOptions: FloatValues.type): RandomFloatOnRQuery = new RandomFloatOnRQuery {
      val arguments = upperBound :: Nil
      val options = randomOptions
    }

    /*def random(upperBound: ReqlFloat,
               randomOptions: FloatValues.type): RandomFloatOnRQuery = new RandomFloatOnRQuery {
      val arguments = upperBound :: Nil
      val options = randomOptions
    }*/

    def random(lowerBound: ReqlInteger,
               upperBound: ReqlInteger): RandomIntegerOnRQuery = new RandomIntegerOnRQuery {
      val arguments = lowerBound :: upperBound :: Nil
      val options = Options.empty
    }

    def random(lowerBound: ReqlInteger,
               upperBound: ReqlInteger,
               randomOptions: IntegerValues.type): RandomIntegerOnRQuery = new RandomIntegerOnRQuery {
      val arguments = lowerBound :: upperBound :: Nil
      val options = randomOptions
    }

    def random(lowerBound: ReqlNumber,
               upperBound: ReqlNumber,
               randomOptions: FloatValues.type): RandomFloatOnRQuery = new RandomFloatOnRQuery {
      val arguments = lowerBound :: upperBound :: Nil
      val options = randomOptions
    }

    /*def random(lowerBound: ReqlFloat,
               upperBound: ReqlFloat,
               randomOptions: FloatValues.type): RandomFloatOnRQuery = new RandomFloatOnRQuery {
      val arguments = lowerBound :: upperBound :: Nil
      val options = randomOptions
    }*/
  }

}
