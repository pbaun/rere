package rere.ql.queries

import rere.ql.types.{ReqlPrimitiveExpr, ReqlR}

trait RQuery {

  sealed class ReqlRQuery extends ReqlR with ReqlPrimitiveExpr {
    def string = "r"
    def repr = "<<r>>"
  }

  object r extends ReqlRQuery

}
