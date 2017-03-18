package rere.ql.queries

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait LogicQueries {

  trait ReqlBiCompareOper extends ReqlBoolean

  // and
  trait AndQuery extends ReqlBoolean

  implicit class AndOnROp(val r: ReqlR) {
    def and(booleans: ReqlBoolean*): AndQuery = new AndQuery {
      val command = TermType.AND
      val string = "and"
      val arguments = booleans.toList
      val options = Options.empty
    }
  }

  implicit class AndOnBooleanOp(val boolean: ReqlBoolean) {
    def and(booleans: ReqlBoolean*): AndQuery = new AndQuery {
      val command = TermType.AND
      val string = "and"
      val arguments = boolean :: booleans.toList
      val options = Options.empty
    }
  }

  // or
  trait OrQuery extends ReqlBoolean

  implicit class OrOnROp(val r: ReqlR) {
    def or(booleans: ReqlBoolean*): OrQuery = new OrQuery {
      val command = TermType.OR
      val string = "or"
      val arguments = booleans.toList
      val options = Options.empty
    }
  }

  implicit class OrOnBooleanOp(val boolean: ReqlBoolean) {
    def or(booleans: ReqlBoolean*): OrQuery = new OrQuery {
      val command = TermType.OR
      val string = "or"
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

  implicit class EqOnValueOp(value: ReqlDatum) {
    def eq_(otherValue: ReqlDatum): EqQuery = new EqQuery {
      val arguments = value :: otherValue :: Nil
    }

    def eq_(values: Iterable[ReqlDatum]): EqQuery = new EqQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class EqOnROp(r: ReqlR) {
    def eq_(values: Iterable[ReqlDatum]): EqQuery = new EqQuery {
      val arguments = values.toList
    }
  }

  // ne
  trait NeQuery extends ReqlBiCompareOper {
    val command = TermType.NE
    val string = "!="
    val options = Options.empty
  }

  implicit class NeOnValueOp(value: ReqlDatum) {
    def ne_(otherValue: ReqlDatum): NeQuery = new NeQuery {
      val arguments = value :: otherValue :: Nil
    }

    def ne_(values: Iterable[ReqlDatum]): NeQuery = new NeQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class NeOnROp(r: ReqlR) {
    def ne_(values: Iterable[ReqlDatum]): NeQuery = new NeQuery {
      val arguments = values.toList
    }
  }

  // lt
  trait LtQuery extends ReqlBiCompareOper {
    val command = TermType.LT
    val string = "<"
    val options = Options.empty
  }

  implicit class LtOnValueOp(value: ReqlDatum) {
    def lt(otherValue: ReqlDatum): LtQuery = new LtQuery {
      val arguments = value :: otherValue :: Nil
    }

    def lt(values: Iterable[ReqlDatum]): LtQuery = new LtQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class LtOnROp(r: ReqlR) {
    def lt(values: Iterable[ReqlDatum]): LtQuery = new LtQuery {
      val arguments = values.toList
    }
  }

  // le
  trait LeQuery extends ReqlBiCompareOper {
    val command = TermType.LE
    val string = "<="
    val options = Options.empty
  }

  implicit class LeOnValueOp(value: ReqlDatum) {
    def le(otherValue: ReqlDatum): LeQuery = new LeQuery {
      val arguments = value :: otherValue :: Nil
    }

    def le(values: Iterable[ReqlDatum]): LeQuery = new LeQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class LeOnROp(r: ReqlR) {
    def le(values: Iterable[ReqlDatum]): LeQuery = new LeQuery {
      val arguments = values.toList
    }
  }

  // gt
  trait GtQuery extends ReqlBiCompareOper {
    val command = TermType.GT
    val string = ">"
    val options = Options.empty
  }

  implicit class GtOnValueOp(value: ReqlDatum) {
    def gt(otherValue: ReqlDatum): GtQuery = new GtQuery {
      val arguments = value :: otherValue :: Nil
    }

    def gt(values: Iterable[ReqlDatum]): GtQuery = new GtQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class GtOnROp(r: ReqlR) {
    def gt(values: Iterable[ReqlDatum]): GtQuery = new GtQuery {
      val arguments = values.toList
    }
  }

  // ge
  trait GeQuery extends ReqlBiCompareOper {
    val command = TermType.GE
    val string = ">="
    val options = Options.empty
  }

  implicit class GeOnValueOp(value: ReqlDatum) {
    def ge(otherValue: ReqlDatum): GeQuery = new GeQuery {
      val arguments = value :: otherValue :: Nil
    }

    def ge(values: Iterable[ReqlDatum]): GeQuery = new GeQuery {
      val arguments = value :: values.toList
    }
  }

  implicit class GeOnROp(r: ReqlR) {
    def ge(values: Iterable[ReqlDatum]): GeQuery = new GeQuery {
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
