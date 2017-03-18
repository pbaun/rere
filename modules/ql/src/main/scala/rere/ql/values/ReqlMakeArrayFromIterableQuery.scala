package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types.{ReqlArray, ReqlDatum}

class ReqlMakeArrayFromIterableQuery[T <: ReqlDatum](iterable: Iterable[T]) extends ReqlArray[T] {
  def command = TermType.MAKE_ARRAY
  def string = "make_array"
  def arguments = iterable.toList
  def options = Options.empty
}
