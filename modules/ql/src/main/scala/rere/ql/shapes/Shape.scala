package rere.ql.shapes

import io.circe.{Decoder, Json, ObjectEncoder}
import rere.ql.types._
import rere.ql.values.{ReqlJsonObjectQuery, ReqlMakeObjFromMapQuery}
import shapeless.ops.function.{FnFromProduct, FnToProduct}
import shapeless.{::, HList, HNil}

trait ModelShape[Model] {
  def toReqlObject(model: Model): ReqlObject
  def fromJson(json: Json): ModelShape.DecodingResult[Model]
}

object ModelShape {
  type DecodingResult[Model] = Either[ShapeDecodingError, Model]

  def apply[Model](implicit shape: ModelShape[Model]): ModelShape[Model] = shape

  implicit def shapeAccessor[Model](implicit tableDescriptor: TableDescriptor[Model]): ModelShape[Model] = {
    tableDescriptor.shape
  }
}

case class ShapeDecodingError(message: String, json: Json)

abstract class Shape[Constructor <: AnyRef, Args <: HList, ModelType](
    constructor: Constructor)(
    implicit deconstruction: FnToProduct.Aux[Constructor, Args => ModelType]
  ) extends ModelShape[ModelType] {

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

  type FieldAux[K, V] = Field[K] { type Value = V }

  def field[FieldType, DatumType <: ReqlDatum](
    name: String,
    valueAccessor: Model => FieldType)(
    implicit fieldLift: FieldLift.Aux[FieldType, DatumType]
  ): FieldAux[name.type, FieldType] = {
    new Field[name.type] {
      override type Root = RootRef
      override type Value = FieldType
      override type Datum = DatumType

      override def key: String = name
      override def accessor: Model => Value = valueAccessor
      override def lift: FieldLift.Aux[Value, Datum] = fieldLift
    }
  }

  def sub[FieldType](
    name: String,
    valueAccessor: Model => FieldType,
    subShape: ModelShape[FieldType]
  ): FieldAux[name.type, FieldType] = {
    new Field[name.type] {
      override type Root = RootRef
      override type Value = FieldType
      override type Datum = ReqlObject

      override def key: String = name
      override def accessor: Model => Value = valueAccessor
      override def lift: FieldLift.Aux[Value, Datum] = FieldLift.liftFromShape(subShape)
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

    def simpleList: List[FieldAux[_, _]]

    def argsProducer: Json => ModelShape.DecodingResult[ArgsList]

    def verifiableShape(f: FunctionType): VerifiableShape[ArgsList, FunctionType]

    final def :-:[K, V, FnType](
      field: FieldAux[K, V])(
      implicit fnConstruction: FnFromProduct.Aux[V :: ArgsList => Model, FnType]
    ): ShapeBlueprintAux[V :: ArgsList, FnType] = {
      new ShapeBlueprintCons[K, V, ArgsList, FnType](field :: simpleList, field, this)
    }
  }

  type ShapeBlueprintAux[ArgsList <: HList, FnType] = ShapeBlueprint[ArgsList] { type FunctionType = FnType }

  object SNil extends ShapeBlueprint[HNil] {

    override type FunctionType = Unit => Model

    override def simpleList: List[FieldAux[_, _]] = Nil

    override def argsProducer: Json => ModelShape.DecodingResult[HNil] = { _ => Right(HNil) }

    override def verifiableShape(f: FunctionType): VerifiableShape[HNil, FunctionType] = {
      VerifiableShape(this, f)
    }
  }

  class ShapeBlueprintCons[HeadName, HeadValue, PrevArgs <: HList, FnType](
      val simpleList: List[FieldAux[_, _]],
      headField: FieldAux[HeadName, HeadValue],
      prev: ShapeBlueprint[PrevArgs]
    ) extends ShapeBlueprint[HeadValue :: PrevArgs] {

    type FunctionType = FnType

    override def argsProducer: Json => ModelShape.DecodingResult[HeadValue :: PrevArgs] = { json =>
      //TODO: trampoline?
      headField.fromJson(json) match {
        case Right(headValue) =>
          prev.argsProducer(json) match {
            case Right(other) => Right(headValue :: other)
            case Left(err) => Left(err)
          }
        case Left(err) => Left(err)
      }
    }

    override def verifiableShape(f: FunctionType): VerifiableShape[HeadValue :: PrevArgs, FunctionType] = {
      VerifiableShape(this, f)
    }
  }

  final def toMap(model: Model): Map[String, Any] = {
    projection.blueprint.simpleList.map(_.toPair(model)).toMap
  }

  final override def toReqlObject(model: Model): ReqlObject = {
    new ReqlMakeObjFromMapQuery(projection.blueprint.simpleList.map(_.toReqlPair(model)).toMap)
  }

  final override def fromJson(json: Json): ModelShape.DecodingResult[ModelType] = {
    projection.blueprint.argsProducer(json) match {
      case Right(args) =>
        val f = FnToProduct[Constructor].apply(constructor)
        Right(f(args))

      case Left(err) => Left(err)
    }
  }

  def projection: Projection

}

abstract class CirceShape[M](implicit objEncoder: ObjectEncoder[M], decoder: Decoder[M])
  extends ModelShape[M] {

  override def toReqlObject(model: M): ReqlObject = {
    new ReqlJsonObjectQuery(objEncoder.encodeObject(model))
  }

  override def fromJson(json: Json): ModelShape.DecodingResult[M] = {
    decoder.decodeJson(json) match {
      case Right(m) => Right(m)
      case Left(err) => Left(ShapeDecodingError(err.message, json))
    }
  }
}
