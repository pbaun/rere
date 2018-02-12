package rere.ql.shapes

import java.util.UUID

import cats.free.Trampoline
import cats.free.Trampoline._
import cats.instances.function._
import io.circe._
import rere.ql.types._
import rere.ql.values.{ReqlJsonObjectQuery, ReqlMakeObjFromPairsListQuery}
import rere.ql.wire.ReqlDecoder
import shapeless.ops.function.{FnFromProduct, FnToProduct}
import shapeless.{::, HList, HNil}

trait ModelShape[Model, Key <: PrimaryKey] {
  def toReqlObject(model: Model): ReqlObject
  def toReqlUnidentifiableObject(model: Model): ReqlObject
  def fromJson(json: Json): ModelShape.DecodingResult[Model]
}

object ModelShape extends LowPriorityModelShape {
  type DecodingResult[Model] = Either[ShapeDecodingError, Model]

  def apply[Model, Key <: PrimaryKey](implicit shape: ModelShape[Model, Key]): ModelShape[Model, Key] = shape

  implicit def shapableToShape[Model, Key <: PrimaryKey](
    implicit shapable: ReqlShapable[Model, Key]
  ): ModelShape[Model, Key] = {
    shapable.shape
  }

  def decodingResultConverter[T](result: ReqlDecoder.Result[T], json: Json): DecodingResult[T] = {
    result match {
      case Right(m) => Right(m)
      case Left(err) => Left(ShapeDecodingError(err.message, json))
    }
  }
}

trait LowPriorityModelShape {
  //TODO: make helper for custom shapes
  implicit def defaultJsonObjectShape: ModelShape[JsonObject, PrimaryKey.String] = {
    new ModelShape[JsonObject, PrimaryKey.String] {
      override def toReqlObject(model: JsonObject): ReqlObject = {
        new ReqlJsonObjectQuery(model)
      }
      override def toReqlUnidentifiableObject(model: JsonObject): ReqlObject = {
        new ReqlJsonObjectQuery(model.filter { case (key, _) => key != "id" })
      }
      override def fromJson(json: Json): ModelShape.DecodingResult[JsonObject] = {
        ReqlDecoder.jsonObjectReqlDecoder.decode(json) match {
          case Right(m) => Right(m)
          case Left(err) => Left(ShapeDecodingError(err.message, json))
        }
      }
    }
  }
}

case class ShapeDecodingError(message: String, json: Json)

abstract class Shape[Constructor <: AnyRef, Args <: HList, ModelType, PrimaryKeyType <: PrimaryKey](
    constructor: Constructor,
    shapePrimaryKey: PK[PrimaryKeyType]
  )(
    implicit deconstruction: FnToProduct.Aux[Constructor, Args => ModelType]
  ) extends ModelShape[ModelType, PrimaryKeyType] {

  type RootRef = this.type
  type Model = ModelType

  trait Field[Name] {
    type Root
    type Value
    type Datum <: ReqlDatum

    def key: String
    def accessor: Model => Value
    def lift: FieldLift.Aux[Value, Datum]

    final def toPair(model: Model): (String, Value) = (key, accessor(model))
    final def toReqlPair(model: Model): (String, Datum) = (key, lift.getEncoder.encode(accessor(model)))

    final def fromJson(json: Json): ModelShape.DecodingResult[Value] = {
      json.asObject match {
        case Some(jsonObj) =>
          jsonObj(key) match {
            case Some(fieldJson) =>
              lift.getDecoder.decode(fieldJson) match {
                case Right(x) => Right(x)
                case Left(err) => Left(ShapeDecodingError(s"Field '$key': ${err.message}", fieldJson))
              }

            case _ => Left(ShapeDecodingError(s"Field '$key' is missing", json))
          }

        case _ => Left(ShapeDecodingError(s"Json value is not an object", json))
      }
    }
  }

  type FieldAux[K, V, D <: ReqlDatum] = Field[K] {
    type Value = V
    type Datum = D
  }

  def field[FieldType, DatumType <: ReqlDatum](
    name: String,
    valueAccessor: Model => FieldType)(
    implicit fieldLift: FieldLift.Aux[FieldType, DatumType]
  ): FieldAux[name.type, FieldType, DatumType] = {
    new Field[name.type] {
      override type Root = RootRef
      override type Value = FieldType
      override type Datum = DatumType

      override def key: String = name
      override def accessor: Model => Value = valueAccessor
      override def lift: FieldLift.Aux[Value, Datum] = fieldLift
    }
  }

  def sub[FieldType, Key <: PrimaryKey](
    name: String,
    valueAccessor: Model => FieldType,
    subShape: ModelShape[FieldType, Key]
  ): FieldAux[name.type, FieldType, ReqlModel[FieldType, Key]] = {
    new Field[name.type] {
      override type Root = RootRef
      override type Value = FieldType
      override type Datum = ReqlModel[FieldType, Key]

      override def key: String = name
      override def accessor: Model => Value = valueAccessor
      override def lift: FieldLift.Aux[Value, Datum] = FieldLift.liftFromShape(subShape)
    }
  }

  trait PrimaryKeyShape[ReqlPK <: ReqlDatum, ScalaPK] {
    def keyFields: Set[String]
  }

  def pk[Name, FieldType, DatumType <: ReqlDatum](field: FieldAux[Name, FieldType, DatumType]): PrimaryKeyShape[DatumType, FieldType] = {
    new PrimaryKeyShape[DatumType, FieldType] {
      override def keyFields = Set(field.key)
    }
  }

  def auto: PrimaryKeyShape[ReqlUUID, UUID] = {
    new PrimaryKeyShape[ReqlUUID, UUID] {
      override def keyFields = Set("id")
    }
  }

  implicit class ByNameGetter(model: Model) {
    def get(name: String)(implicit field: Field[name.type]): field.Value = {
      field.accessor(model)
    }
  }

  def getField(model: Model, fieldName: String)(implicit field: Field[fieldName.type]): field.Value = {
    field.accessor(model)
  }


  case class VerifiableShape[ArgsList <: HList, FnType](blueprint: ShapeBlueprint[ArgsList], f: FnType) {
    def list = blueprint.simpleList
  }

  type Projection = VerifiableShape[Args, Constructor]

  implicit def autoVerifier(shape: ShapeBlueprintAux[Args, Constructor]): Projection = {
    shape.verifiableShape(constructor)
  }

  trait ShapeBlueprint[ArgsList <: HList] {
    type FunctionType

    def simpleList: List[FieldAux[_, _, _ <: ReqlDatum]]

    def argsProducer(json: Json): Trampoline[ModelShape.DecodingResult[ArgsList]]

    def verifiableShape(f: FunctionType): VerifiableShape[ArgsList, FunctionType]

    final def :-:[K, V, D <: ReqlDatum, FnType](
      field: FieldAux[K, V, D])(
      implicit fnConstruction: FnFromProduct.Aux[V :: ArgsList => Model, FnType]
    ): ShapeBlueprintAux[V :: ArgsList, FnType] = {
      new ShapeBlueprintCons[K, V, D, ArgsList, FnType](field :: simpleList, field, this)
    }
  }

  type ShapeBlueprintAux[ArgsList <: HList, FnType] = ShapeBlueprint[ArgsList] { type FunctionType = FnType }

  object SNil extends ShapeBlueprint[HNil] {

    override type FunctionType = Unit => Model

    override def simpleList: List[FieldAux[_, _, _ <: ReqlDatum]] = Nil

    override def argsProducer(json: Json): Trampoline[ModelShape.DecodingResult[HNil]] = {
      done(Right(HNil))
    }

    override def verifiableShape(f: FunctionType): VerifiableShape[HNil, FunctionType] = {
      VerifiableShape(this, f)
    }
  }

  class ShapeBlueprintCons[HeadName, HeadValue, HeadDatum <: ReqlDatum, TailValues <: HList, FnType](
      val simpleList: List[FieldAux[_, _, _ <: ReqlDatum]],
      headField: FieldAux[HeadName, HeadValue, HeadDatum],
      tailBlueprint: ShapeBlueprint[TailValues]
    ) extends ShapeBlueprint[HeadValue :: TailValues] {

    type FunctionType = FnType

    override def argsProducer(json: Json): Trampoline[ModelShape.DecodingResult[HeadValue :: TailValues]] = {
      for {
        head <- done(headField.fromJson(json))
        tail <- defer(tailBlueprint.argsProducer(json))
        list <- done {
          head match {
            case Right(headValue) =>
              tail match {
                case Right(tailValues) => Right(headValue :: tailValues)
                case Left(err) => Left(err)
              }
            case Left(err) => Left(err)
          }
        }
      } yield list
    }

    override def verifiableShape(f: FunctionType): VerifiableShape[HeadValue :: TailValues, FunctionType] = {
      VerifiableShape(this, f)
    }
  }

  final override def toReqlObject(model: Model): ReqlObject = {
    new ReqlMakeObjFromPairsListQuery(projection.blueprint.simpleList.map(_.toReqlPair(model)))
  }

  final override def toReqlUnidentifiableObject(model: Model): ReqlObject = {
    new ReqlMakeObjFromPairsListQuery(
      projection.blueprint.simpleList.filter { field =>
        !primaryKey.keyFields.contains(field.key)
      }.map(_.toReqlPair(model))
    )
  }

  final override def fromJson(json: Json): ModelShape.DecodingResult[ModelType] = {
    projection.blueprint.argsProducer(json).run match {
      case Right(args) =>
        Right(FnToProduct[Constructor].apply(constructor)(args))

      case Left(err) => Left(err)
    }
  }

  def primaryKey: PrimaryKeyShape[PrimaryKeyType#Reql, PrimaryKeyType#Scala]

  def projection: Projection

}

abstract class CirceShape[M, Key <: PrimaryKey](
    implicit modelEncoder: ObjectEncoder[M],
    modelDecoder: Decoder[M]
  ) extends ModelShape[M, Key] {

  def primaryKey: Set[String] = Set("id")

  override def toReqlObject(model: M): ReqlObject = {
    new ReqlJsonObjectQuery(modelEncoder.encodeObject(model))
  }

  override def toReqlUnidentifiableObject(model: M): ReqlObject = {
    new ReqlJsonObjectQuery(modelEncoder.encodeObject(model).filterKeys(!primaryKey.contains(_)))
  }

  override def fromJson(json: Json): ModelShape.DecodingResult[M] = {
    modelDecoder.decodeJson(json) match {
      case Right(m) => Right(m)
      case Left(err) => Left(ShapeDecodingError(err.message, json))
    }
  }
}
