package rere.ql.typeclasses

import rere.ql.types._
import rere.ql.util.{FunctionProxyQuery, ProxyQuery}

import scala.annotation.implicitNotFound

@implicitNotFound("Very likely type ${In} cannot be used as predicate")
trait ToPredicate[In] {
  type Out <: ReqlDatum
  def toPredicate(in: In): ReqlPredicate[Out]
}

object ToPredicate {
  @implicitNotFound("Very likely type ${In} cannot be used as predicate of type ${Out0}")
  type Aux[In, Out0] = ToPredicate[In] { type Out = Out0 }

  type Projection[T] = Aux[T, T]

  def apply[In](in: In)(implicit conv: ToPredicate[In]): ReqlPredicate[conv.Out] = {
    conv.toPredicate(in)
  }

  implicit def datumToPredicate[In <: ReqlDatum]: Projection[In] = {
    new ToPredicate[In] {
      final type Out = In
      override def toPredicate(in: In): ReqlPredicate[Out] = {
        new ProxyQuery(in) with ReqlPredicate[Out]
      }
    }
  }

  implicit def functionToPredicate[In <: ReqlDatum : Transmuter]: Aux[In => ReqlBoolean, In] = {
    new ToPredicate[In => ReqlBoolean] {
      final type Out = In
      override def toPredicate(in: In => ReqlBoolean): ReqlPredicate[Out] = {
        new FunctionProxyQuery[Out, ReqlBoolean](in) with ReqlPredicate[Out]
      }
    }
  }

}
