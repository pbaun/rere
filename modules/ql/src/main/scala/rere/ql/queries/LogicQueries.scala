package rere.ql.queries

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait LogicQueries {

  trait ReqlBiCompareOper extends ReqlBoolean

  // or
  trait OrQuery extends ReqlBoolean

  implicit class OrOnROp(r: ReqlR) {
    def or(booleans: ReqlBoolean*): OrQuery = new OrQuery {
      val command = TermType.OR
      val string = "or"
      val arguments = booleans.toList
      val options = Options.empty
    }
  }

  implicit class OrOnBooleanOp(boolean: ReqlBoolean) {
    def or(booleans: ReqlBoolean*): OrQuery = new OrQuery {
      val command = TermType.OR
      val string = "or"
      val arguments = boolean :: booleans.toList
      val options = Options.empty
    }
  }

  // and
  trait AndQuery extends ReqlBoolean

  implicit class AndOnROp(r: ReqlR) {
    def and(booleans: ReqlBoolean*): AndQuery = new AndQuery {
      val command = TermType.AND
      val string = "and"
      val arguments = booleans.toList
      val options = Options.empty
    }
  }

  implicit class AndOnBooleanOp(boolean: ReqlBoolean) {
    def and(booleans: ReqlBoolean*): AndQuery = new AndQuery {
      val command = TermType.AND
      val string = "and"
      val arguments = boolean :: booleans.toList
      val options = Options.empty
    }
  }

  // eq
  trait EqQuery extends ReqlBiCompareOper {
    val command = TermType.EQ
    val string = "=="
    val options = Options.empty
  }

  implicit class EqOnValueOp[T <: ReqlDatum](value: T) {
    def eq_(values: T*): EqQuery = new EqQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class EqOnROp(r: ReqlR) {
    def eq_[T <: ReqlDatum](values: T*): EqQuery = new EqQuery {
      val arguments = values.toList
    }
  }

  // ne
  trait NeQuery extends ReqlBiCompareOper {
    val command = TermType.NE
    val string = "!="
    val options = Options.empty
  }

  implicit class NeOnValueOp[T <: ReqlDatum](value: T) {
    def ne_(values: T*): NeQuery = new NeQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class NeOnROp(r: ReqlR) {
    def ne_[T <: ReqlDatum](values: T*): NeQuery = new NeQuery {
      val arguments = values.toList
    }
  }

  // lt
  trait LtQuery extends ReqlBiCompareOper {
    val command = TermType.LT
    val string = "<"
    val options = Options.empty
  }

  implicit class LtOnValueOp[T <: ReqlDatum](value: T) {
    def lt(values: T*): LtQuery = new LtQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class LtOnROp(r: ReqlR) {
    def lt[T <: ReqlDatum](values: T*): LtQuery = new LtQuery {
      val arguments = values.toList
    }
  }

  // le
  trait LeQuery extends ReqlBiCompareOper {
    val command = TermType.LE
    val string = "<="
    val options = Options.empty
  }

  implicit class LeOnValueOp[T <: ReqlDatum](value: T) {
    def le(values: T*): LeQuery = new LeQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class LeOnROp(r: ReqlR) {
    def le[T <: ReqlDatum](values: T*): LeQuery = new LeQuery {
      val arguments = values.toList
    }
  }

  // gt
  trait GtQuery extends ReqlBiCompareOper {
    val command = TermType.GT
    val string = ">"
    val options = Options.empty
  }

  implicit class GtOnValueOp[T <: ReqlDatum](value: T) {
    def gt(values: T*): GtQuery = new GtQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class GtOnROp(r: ReqlR) {
    def gt[T <: ReqlDatum](values: T*): GtQuery = new GtQuery {
      val arguments = values.toList
    }
  }

  // ge
  trait GeQuery extends ReqlBiCompareOper {
    val command = TermType.GE
    val string = ">="
    val options = Options.empty
  }

  implicit class GeOnValueOp[T <: ReqlDatum](value: T) {
    def ge(values: T*): GeQuery = new GeQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class GeOnROp(r: ReqlR) {
    def ge[T <: ReqlDatum](values: T*): GeQuery = new GeQuery {
      val arguments = values.toList
    }
  }

  // not
  trait NotQuery extends ReqlExpr {
    val command = TermType.NOT
    val string = "!"
    val options = Options.empty
  }

  trait NotOnBooleanQuery extends NotQuery with ReqlBoolean

  implicit class NotOnBooleanOp(boolean: ReqlBoolean) {
    def not(): NotOnBooleanQuery = new NotOnBooleanQuery {
      val arguments = boolean :: Nil
    }
  }

  implicit class NotOnROp(r: ReqlR) {
    def not(boolean: ReqlBoolean): NotOnBooleanQuery = new NotOnBooleanQuery {
      val arguments = boolean :: Nil
    }
  }

}
