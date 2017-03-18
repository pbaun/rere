package rere.ql.queries

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.typeclasses.{ToUpper, Transmuter}
import rere.ql.types.{ReqlExpr, ReqlNumber}
import rere.ql.values.ReqlMakeArrayFromIterableQuery

class Func(argsIndexes: List[ReqlNumber], body: ReqlExpr) extends ReqlExpr {
  def command = TermType.FUNC
  def string = "func"
  def arguments = new ReqlMakeArrayFromIterableQuery(argsIndexes) :: body :: Nil
  def options = Options.empty
}

object Func {

  def wrap0[
    R <: ReqlExpr
  ](
    func: () => R
  ): ReqlExpr = {
    new Func(Nil, func())
  }

  def wrap1[
    T0 <: ReqlExpr,
    R <: ReqlExpr
  ](
    func: T0 => R
  )(
    implicit
    toT0: Transmuter[T0]
  ): ReqlExpr = {
    val argsIndexes = values.expr(0) :: Nil
    val arg0: T0 = toT0.transmute(new Var(0))

    new Func(argsIndexes, func(arg0))
  }

  def wrapDo1[
    T0 <: ReqlExpr,
    UpperT0 <: ReqlExpr,
    R <: ReqlExpr
  ](
     func: UpperT0 => R
  )(implicit abc: ToUpper.Aux[T0, UpperT0]): ReqlExpr = {
    val argsIndexes = values.expr(0) :: Nil
    val arg0: UpperT0 = ToUpper[T0](new Var(0))

    new Func(argsIndexes, func(arg0))
  }

  def wrap2[
    T0 <: ReqlExpr,
    T1 <: ReqlExpr,
    R <: ReqlExpr
  ](
    func: (T0, T1) => R
  )(
    implicit
    toT0: Transmuter[T0],
    toT1: Transmuter[T1]
  ): ReqlExpr = {
    val argsIndexes = values.expr(0) :: values.expr(1) :: Nil
    val arg0: T0 = toT0.transmute(new Var(0))
    val arg1: T1 = toT1.transmute(new Var(1))

    new Func(argsIndexes, func(arg0, arg1))
  }

  def wrap3[
    T0 <: ReqlExpr,
    T1 <: ReqlExpr,
    T2 <: ReqlExpr,
    R <: ReqlExpr
  ](
    func: (T0, T1, T2) => R
  )(
    implicit
    toT0: Transmuter[T0],
    toT1: Transmuter[T1],
    toT2: Transmuter[T2]
  ): ReqlExpr = {
    val argsIndexes = values.expr(0) :: values.expr(1) :: values.expr(2) :: Nil
    val arg0: T0 = toT0.transmute(new Var(0))
    val arg1: T1 = toT1.transmute(new Var(1))
    val arg2: T2 = toT2.transmute(new Var(2))

    new Func(argsIndexes, func(arg0, arg1, arg2))
  }

}