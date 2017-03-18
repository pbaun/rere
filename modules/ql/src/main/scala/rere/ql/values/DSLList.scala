package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types.{ReqlArray, ReqlDatum}

trait DSLList[+T <: ReqlDatum] extends ReqlArray[T] {
  def command = TermType.MAKE_ARRAY
  def string = "make_array"
  def options = Options.empty
}
