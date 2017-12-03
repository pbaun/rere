package rere.ql.shapes

import java.time.ZonedDateTime
import java.util.UUID

import akka.util.ByteString
import io.circe.Json
import rere.ql.typeclasses.Transmuter
import rere.ql.types._
import rere.ql.wire.{ReqlDecoder, ReqlEncoder}

import scala.annotation.implicitNotFound

trait FieldLift[FieldType] {
  type ReqlType <: ReqlDatum

  def getEncoder: ReqlEncoder.Aux[FieldType, ReqlType]

  def getDecoder: ReqlDecoder[FieldType]

  def getTransmuter: Transmuter[ReqlType]
}

object FieldLift {

  @implicitNotFound("Very likely driver doesn't support type ${Field} or it support doesn't imported to a shape")
  type Aux[Field, Reql] = FieldLift[Field] {
    type ReqlType = Reql
  }

  def apply[Field](implicit fieldLift: FieldLift[Field]): FieldLift[Field] = fieldLift

  implicit val nullLift: Aux[Null, ReqlNull] = new FieldLift[Null] {
    final type ReqlType = ReqlNull

    override def getEncoder: ReqlEncoder.Aux[Null, ReqlNull] = ReqlEncoder.nullEncoder

    override def getDecoder: ReqlDecoder[Null] = ReqlDecoder.nullReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.nullTransmuter
  }

  implicit val boolLift: Aux[Boolean, ReqlBoolean] = new FieldLift[Boolean] {
    final type ReqlType = ReqlBoolean

    override def getEncoder: ReqlEncoder.Aux[Boolean, ReqlType] = ReqlEncoder.booleanEncoder

    override def getDecoder: ReqlDecoder[Boolean] = ReqlDecoder.booleanReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.booleanTransmuter
  }

  implicit val intLift: Aux[Int, ReqlInteger] = new FieldLift[Int] {
    final type ReqlType = ReqlInteger

    override def getEncoder: ReqlEncoder.Aux[Int, ReqlType] = ReqlEncoder.intEncoder

    override def getDecoder: ReqlDecoder[Int] = ReqlDecoder.intReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.integerTransmuter
  }

  implicit val longLift: Aux[Long, ReqlInteger] = new FieldLift[Long] {
    final type ReqlType = ReqlInteger

    override def getEncoder: ReqlEncoder.Aux[Long, ReqlType] = ReqlEncoder.longEncoder

    override def getDecoder: ReqlDecoder[Long] = ReqlDecoder.longReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.integerTransmuter
  }

  implicit val bigIntLift: Aux[BigInt, ReqlInteger] = new FieldLift[BigInt] {
    final type ReqlType = ReqlInteger

    override def getEncoder: ReqlEncoder.Aux[BigInt, ReqlType] = ReqlEncoder.bigIntEncoder

    override def getDecoder: ReqlDecoder[BigInt] = ReqlDecoder.bigIntReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.integerTransmuter
  }

  implicit val doubleLift: Aux[Double, ReqlFloat] = new FieldLift[Double] {
    final type ReqlType = ReqlFloat

    override def getEncoder: ReqlEncoder.Aux[Double, ReqlType] = ReqlEncoder.doubleEncoder

    override def getDecoder: ReqlDecoder[Double] = ReqlDecoder.doubleReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.floatTransmuter
  }

  implicit val bigDecimalLift: Aux[BigDecimal, ReqlFloat] = new FieldLift[BigDecimal] {
    final type ReqlType = ReqlFloat

    override def getEncoder: ReqlEncoder.Aux[BigDecimal, ReqlType] = ReqlEncoder.bigDecimalEncoder

    override def getDecoder: ReqlDecoder[BigDecimal] = ReqlDecoder.bigDecimalReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.floatTransmuter
  }

  implicit val stringLift: Aux[String, ReqlString] = new FieldLift[String] {
    final type ReqlType = ReqlString

    override def getEncoder: ReqlEncoder.Aux[String, ReqlType] = ReqlEncoder.stringEncoder

    override def getDecoder: ReqlDecoder[String] = ReqlDecoder.stringReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.stringTransmuter
  }

  implicit val zonedDateTimeLift: Aux[ZonedDateTime, ReqlTime] = new FieldLift[ZonedDateTime] {
    final type ReqlType = ReqlTime

    override def getEncoder: ReqlEncoder.Aux[ZonedDateTime, ReqlType] = ReqlEncoder.zonedDateTimeEncoder

    override def getDecoder: ReqlDecoder[ZonedDateTime] = ReqlDecoder.zonedDataTimeReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.timeTransmuter
  }

  implicit val uuidLift: Aux[UUID, ReqlUUID] = new FieldLift[UUID] {
    final type ReqlType = ReqlUUID

    override def getEncoder: ReqlEncoder.Aux[UUID, ReqlType] = ReqlEncoder.uuidEncoder

    override def getDecoder: ReqlDecoder[UUID] = ReqlDecoder.uuidReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.uuidTransmuter
  }

  implicit val jsonLift: Aux[Json, ReqlJson] = new FieldLift[Json] {
    final type ReqlType = ReqlJson

    override def getEncoder: ReqlEncoder.Aux[Json, ReqlType] = ReqlEncoder.jsonEncoder

    override def getDecoder: ReqlDecoder[Json] = ReqlDecoder.jsonReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.jsonTransmuter
  }

  implicit val byteStringLift: Aux[ByteString, ReqlBinary] = new FieldLift[ByteString] {
    final type ReqlType = ReqlBinary

    override def getEncoder: ReqlEncoder.Aux[ByteString, ReqlType] = ReqlEncoder.byteStringEncoder

    override def getDecoder: ReqlDecoder[ByteString] = ReqlDecoder.byteStringReqlDecoder

    override def getTransmuter: Transmuter[ReqlType] = Transmuter.binaryTransmuter
  }

  implicit def optionTLift[Field, Reql <: ReqlDatum](
    implicit
    innerLift: FieldLift.Aux[Field, Reql]
  ): Aux[Option[Field], Reql] = {
    new FieldLift[Option[Field]] {
      final type ReqlType = Reql

      override def getEncoder: ReqlEncoder.Aux[Option[Field], ReqlType] = {
        ReqlEncoder.optionEncoder(innerLift.getEncoder, innerLift.getTransmuter)
      }

      override def getDecoder: ReqlDecoder[Option[Field]] = {
        ReqlDecoder.optionReqlDecoder(innerLift.getDecoder)
      }

      override def getTransmuter: Transmuter[ReqlType] = innerLift.getTransmuter
    }
  }

  implicit def seqTLift[Field, Reql <: ReqlDatum](
    implicit
    innerLift: FieldLift.Aux[Field, Reql]
  ): Aux[Seq[Field], ReqlArray[Reql]] = {
    new FieldLift[Seq[Field]] {
      final type ReqlType = ReqlArray[Reql]

      override def getEncoder: ReqlEncoder.Aux[Seq[Field], ReqlType] = {
        ReqlEncoder.seqEncoder(innerLift.getEncoder)
      }

      override def getDecoder: ReqlDecoder[Seq[Field]] = {
        ReqlDecoder.seqReqlDecoder(innerLift.getDecoder)
      }

      override def getTransmuter: Transmuter[ReqlType] = Transmuter.arrayTransmuter[Reql]
    }
  }

  def liftFromShape[M, Key <: PrimaryKey](shape: ModelShape[M, Key]): FieldLift.Aux[M, ReqlModel[M, Key]] = {
    new FieldLift[M] {
      final type ReqlType = ReqlModel[M, Key]

      override def getEncoder: ReqlEncoder.Aux[M, ReqlType] = {
        ReqlEncoder.modelEncoder(shape)
      }

      override def getDecoder: ReqlDecoder[M] = {
        ReqlDecoder.modelReqlDecoder(shape)
      }

      override def getTransmuter: Transmuter[ReqlType] = Transmuter.modelTransmuter(shape)
    }
  }

}
