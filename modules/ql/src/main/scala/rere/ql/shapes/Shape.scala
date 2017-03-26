package rere.ql.shapes

import cats.free.Trampoline
import cats.free.Trampoline._
import cats.instances.function._
import io.circe.{Decoder, Json, ObjectEncoder}
import rere.ql.types._
import rere.ql.values.{ReqlJsonObjectQuery, ReqlMakeObjFromPairsListQuery}
import shapeless.ops.function.{FnFromProduct, FnToProduct}
import shapeless.{::, HList, HNil}

trait ModelShape[Model, PK] {
  def toReqlObject(model: Model): ReqlObject
  def toReqlUnidentifiableObject(model: Model): ReqlObject
  def fromJson(json: Json): ModelShape.DecodingResult[Model]
}

object ModelShape {
  type DecodingResult[Model] = Either[ShapeDecodingError, Model]

  def apply[Model, PK](implicit shape: ModelShape[Model, PK]): ModelShape[Model, PK] = shape

  implicit def shapeAccessor[Model, PK](implicit tableDescriptor: TableDescriptor[Model, PK]): ModelShape[Model, PK] = {
    tableDescriptor.shape
  }
}

case class ShapeDecodingError(message: String, json: Json)

abstract class Shape[Constructor <: AnyRef, Args <: HList, ModelType, PrimaryKeyType](
    constructor: Constructor, shapePrimaryKey: ShapePrimaryKey[PrimaryKeyType])(
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

  def sub[FieldType, PK](
    name: String,
    valueAccessor: Model => FieldType,
    subShape: ModelShape[FieldType, PK]
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

  trait PrimaryKeyShape[T] {
    def keyFields: Set[String]
  }

  def pk[Name, FieldType](field: FieldAux[Name, FieldType]): PrimaryKeyShape[FieldType] = {
    new PrimaryKeyShape[FieldType] {
      def keyFields = Set(field.key)
    }
  }

  def noPk: PrimaryKeyShape[Nothing] = {
    new PrimaryKeyShape[Nothing] {
      def keyFields = Set.empty
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

    def argsProducer(json: Json): Trampoline[ModelShape.DecodingResult[ArgsList]]

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

    override def argsProducer(json: Json): Trampoline[ModelShape.DecodingResult[HNil]] = {
      done(Right(HNil))
    }

    override def verifiableShape(f: FunctionType): VerifiableShape[HNil, FunctionType] = {
      VerifiableShape(this, f)
    }
  }

  class ShapeBlueprintCons[HeadName, HeadValue, TailValues <: HList, FnType](
      val simpleList: List[FieldAux[_, _]],
      headField: FieldAux[HeadName, HeadValue],
      tailBlueprint: ShapeBlueprint[TailValues]
    ) extends ShapeBlueprint[HeadValue :: TailValues] {

    type FunctionType = FnType

    override def argsProducer(json: Json): Trampoline[ModelShape.DecodingResult[HeadValue :: TailValues]] = {
      for {
        head <- done(headField.fromJson(json))
        tail <- suspend(tailBlueprint.argsProducer(json))
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

  /*final def toMap(model: Model): Map[String, Any] = {
    projection.blueprint.simpleList.map(_.toPair(model)).toMap
  }*/

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

  def primaryKey: PrimaryKeyShape[PrimaryKeyType]

  def projection: Projection

}

abstract class CirceShape[M, PK](implicit objEncoder: ObjectEncoder[M], decoder: Decoder[M])
  extends ModelShape[M, PK] {

  def primaryKey: Set[String] = Set("id")

  override def toReqlObject(model: M): ReqlObject = {
    new ReqlJsonObjectQuery(objEncoder.encodeObject(model))
  }

  override def toReqlUnidentifiableObject(model: M): ReqlObject = {
    new ReqlJsonObjectQuery(objEncoder.encodeObject(model).filterKeys(!primaryKey.contains(_)))
  }

  override def fromJson(json: Json): ModelShape.DecodingResult[M] = {
    decoder.decodeJson(json) match {
      case Right(m) => Right(m)
      case Left(err) => Left(ShapeDecodingError(err.message, json))
    }
  }
}
