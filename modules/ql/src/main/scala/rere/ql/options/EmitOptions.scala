package rere.ql.options

import rere.ql.queries.Func
import rere.ql.typeclasses.Transmuter
import rere.ql.types.{ReqlArray, ReqlDatum}

trait EmitOptions {

  sealed trait EmitOptions[R <: ReqlDatum, Base <: ReqlDatum] extends ComposableOptions

  // usually compiler can't infer this types and they should be specified explicitly
  case class Emit[
    R <: ReqlDatum : Transmuter,
    Base <: ReqlDatum : Transmuter,
    Out <: ReqlDatum
  ](
    emitFunction: (R, Base, R) => ReqlArray[Out]
  ) extends EmitOptions[R, Base] with NonDefaultOption {

    def withFinalEmit(finalEmit: R => ReqlArray[Out]): EmitWithFinalEmit[R, Base, Out] = EmitWithFinalEmit(emitFunction, finalEmit)

    def view = "emit" -> Func.wrap3(emitFunction) :: Nil
  }

  // usually compiler can't infer this types and they should be specified explicitly
  case class EmitWithFinalEmit[
    R <: ReqlDatum : Transmuter,
    Base <: ReqlDatum : Transmuter,
    Out <: ReqlDatum
  ](
    emitFunction: (R, Base, R) => ReqlArray[Out],
    finalEmit: R => ReqlArray[Out]
  ) extends EmitOptions[R, Base] with NonDefaultOption {

    def view = "emit" -> Func.wrap3(emitFunction) :: "final_emit" -> Func.wrap1(finalEmit) :: Nil
  }

}
