package rere.ql.typeclasses

import rere.ql.types._
import rere.ql.util.{FunctionProxyQuery, ProxyQuery}

import scala.annotation.implicitNotFound

@implicitNotFound("Very likely type ${In} cannot be used as predicate")
trait ToFilterPredicate[In] {
  type Out <: ReqlDatum
  def toFilterPredicate(in: In): ReqlFilterPredicate[Out]
}

object ToFilterPredicate {

  type Aux[In, Out0] = ToFilterPredicate[In] { type Out = Out0 }

  type Projection[T] = Aux[T, T]

  def apply[In](in: In)(implicit conv: ToFilterPredicate[In]): ReqlFilterPredicate[conv.Out] = {
    conv.toFilterPredicate(in)
  }

  implicit def objectToFilterPredicate[In <: ReqlObject]: Projection[In] = {
    new ToFilterPredicate[In] {
      final type Out = In
      override def toFilterPredicate(in: In): ReqlFilterPredicate[Out] = {
        new ProxyQuery(in) with ReqlFilterPredicate[Out]
      }
    }
  }

  implicit def functionToFilterPredicate[In <: ReqlDatum : Transmuter]: Aux[In => ReqlBoolean, In] = {
    new ToFilterPredicate[In => ReqlBoolean] {
      final type Out = In
      override def toFilterPredicate(in: In => ReqlBoolean): ReqlFilterPredicate[Out] = {
        new FunctionProxyQuery[Out, ReqlBoolean](in) with ReqlFilterPredicate[Out]
      }
    }
  }

}
