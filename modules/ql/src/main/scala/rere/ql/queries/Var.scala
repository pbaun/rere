package rere.ql.queries

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types.ReqlExpr
import rere.ql.values.ReqlIntQuery

class Var(index: Int) extends ReqlExpr {
  def command = TermType.VAR
  def string = "var"
  def arguments = new ReqlIntQuery(index) :: Nil
  def options = Options.empty
}
