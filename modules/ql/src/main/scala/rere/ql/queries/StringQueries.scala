package rere.ql.queries

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait StringQueries {

  // match
  trait MatchQuery extends ReqlObject
  //TODO: object | null by docs
  //TODO: check it on different queries from .match docs

  implicit class MatchOnStringOp(val str: ReqlString) {
    def match_(regexp: ReqlString): MatchQuery = new MatchQuery {
      val command = TermType.MATCH
      val string = "match"
      val arguments = str :: regexp :: Nil
      val options = Options.empty
    }
  }

  // split
  trait SplitQuery extends ReqlArray[ReqlString]

  implicit class SplitOnStringOp(val str: ReqlString) {
    def split(): SplitQuery = new SplitQuery {
      val command = TermType.SPLIT
      val string = "split"
      val arguments = str :: Nil
      val options = Options.empty
    }

    def split(separator: ReqlString): SplitQuery = new SplitQuery {
      val command = TermType.SPLIT
      val string = "split"
      val arguments = str :: separator :: Nil
      val options = Options.empty
    }

    def split(separator: ReqlString, maxSplits: ReqlInteger): SplitQuery = new SplitQuery {
      val command = TermType.SPLIT
      val string = "split"
      val arguments = str :: separator :: maxSplits :: Nil
      val options = Options.empty
    }

    def split(separator: ReqlNull, maxSplits: ReqlInteger): SplitQuery = new SplitQuery {
      val command = TermType.SPLIT
      val string = "split"
      val arguments = str :: separator :: maxSplits :: Nil
      val options = Options.empty
    }

    def split(maxSplits: ReqlInteger): SplitQuery = new SplitQuery {
      val command = TermType.SPLIT
      val string = "split"
      val arguments = str :: values.expr(null) :: maxSplits :: Nil
      val options = Options.empty
    }
  }

  // upcase
  trait UpcaseQuery extends ReqlString

  implicit class UpcaseOnStringOp(val str: ReqlString) {
    def upcase(): UpcaseQuery = new UpcaseQuery {
      val command = TermType.UPCASE
      val string = "upcase"
      val arguments = str :: Nil
      val options = Options.empty
    }
  }

  // downcase
  trait DowncaseQuery extends ReqlString

  implicit class DowncaseOnStringOp(val str: ReqlString) {
    def downcase(): DowncaseQuery = new DowncaseQuery {
      val command = TermType.DOWNCASE
      val string = "downcase"
      val arguments = str :: Nil
      val options = Options.empty
    }
  }

}
