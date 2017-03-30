package rere.ql.wire

import java.time.ZonedDateTime
import java.util.UUID

import akka.util.ByteString
import io.circe.Json
import rere.ql.shapes.ModelShape
import rere.ql.typeclasses.Transmuter
import rere.ql.types._
import rere.ql.values._

trait ReqlEncoder[ScalaType] {
  type ReqlType <: ReqlDatum

  def encode(value: ScalaType): ReqlType
}

object ReqlEncoder {

  type Aux[Scala, Reql <: ReqlDatum] = ReqlEncoder[Scala] {
    type ReqlType = Reql
  }

  def apply[ScalaType](implicit encoder: ReqlEncoder[ScalaType]): ReqlEncoder[ScalaType] = encoder

  implicit val booleanEncoder: Aux[Boolean, ReqlBoolean] = {
    new ReqlEncoder[Boolean] {
      override type ReqlType = ReqlBoolean

      override def encode(value: Boolean): ReqlType = {
        new ReqlBooleanQuery(value)
      }
    }
  }

  implicit val intEncoder: Aux[Int, ReqlInteger] = {
    new ReqlEncoder[Int] {
      override type ReqlType = ReqlInteger

      override def encode(value: Int): ReqlType = {
        new ReqlIntQuery(value)
      }
    }
  }

  implicit val longEncoder: Aux[Long, ReqlInteger] = {
    new ReqlEncoder[Long] {
      override type ReqlType = ReqlInteger

      override def encode(value: Long): ReqlType = {
        new ReqlLongQuery(value)
      }
    }
  }

  implicit val bigIntEncoder: Aux[BigInt, ReqlInteger] = {
    new ReqlEncoder[BigInt] {
      override type ReqlType = ReqlInteger

      override def encode(value: BigInt): ReqlType = {
        new ReqlBigIntQuery(value)
      }
    }
  }

  implicit val doubleEncoder: Aux[Double, ReqlFloat] = {
    new ReqlEncoder[Double] {
      override type ReqlType = ReqlFloat

      override def encode(value: Double): ReqlType = {
        new ReqlBigDecimalQuery(value)
      }
    }
  }

  implicit val bigDecimalEncoder: Aux[BigDecimal, ReqlFloat] = {
    new ReqlEncoder[BigDecimal] {
      override type ReqlType = ReqlFloat

      override def encode(value: BigDecimal): ReqlType = {
        new ReqlBigDecimalQuery(value)
      }
    }
  }

  implicit val stringEncoder: Aux[String, ReqlString] = {
    new ReqlEncoder[String] {
      override type ReqlType = ReqlString

      override def encode(value: String): ReqlType = {
        new ReqlStringQuery(value)
      }
    }
  }

  implicit val zonedDateTimeEncoder: Aux[ZonedDateTime, ReqlTime] = {
    new ReqlEncoder[ZonedDateTime] {
      override type ReqlType = ReqlTime

      override def encode(value: ZonedDateTime): ReqlType = {
        new ReqlTimeQuery(value)
      }
    }
  }

  implicit val uuidEncoder: Aux[UUID, ReqlString] = {
    new ReqlEncoder[UUID] {
      override type ReqlType = ReqlString

      override def encode(value: UUID): ReqlType = {
        new ReqlStringQuery(value.toString)
      }
    }
  }

  implicit val jsonEncoder: Aux[Json, ReqlJson] = {
    new ReqlEncoder[Json] {
      override type ReqlType = ReqlJson

      override def encode(value: Json): ReqlType = {
        new ReqlJsonQuery(value)
      }
    }
  }

  implicit val byteStringEncoder: Aux[ByteString, ReqlBinary] = {
    new ReqlEncoder[ByteString] {
      override type ReqlType = ReqlBinary

      override def encode(value: ByteString): ReqlType = {
        new ReqlBinaryQuery(value)
      }
    }
  }

  implicit def optionEncoder[Scala, Reql <: ReqlDatum](
    implicit
    innerEncoder: ReqlEncoder.Aux[Scala, Reql],
    transmuter: Transmuter[Reql]
  ): Aux[Option[Scala], Reql] = {
    new ReqlEncoder[Option[Scala]] {
      override type ReqlType = Reql

      override def encode(maybeValue: Option[Scala]): ReqlType = {
        maybeValue match {
          case Some(value) => innerEncoder.encode(value)
          case _ => transmuter.transmute(new ReqlNullQuery(null))
        }
      }
    }
  }

  implicit def seqEncoder[Scala, Reql <: ReqlDatum](
    implicit innerEncoder: ReqlEncoder.Aux[Scala, Reql]
  ): Aux[Seq[Scala], ReqlArray[Reql]] = {
    new ReqlEncoder[Seq[Scala]] {
      override type ReqlType = ReqlArray[Reql]

      override def encode(seq: Seq[Scala]): ReqlType = {
        new ReqlMakeArrayFromIterableQuery(seq.map(innerEncoder.encode))
      }
    }
  }

  implicit def modelEncoder[Model, PK](
    implicit modelShape: ModelShape[Model, PK]
  ): Aux[Model, ReqlObject] = {
    new ReqlEncoder[Model] {
      override type ReqlType = ReqlObject

      override def encode(value: Model): ReqlObject = {
        modelShape.toReqlObject(value)
      }
    }
  }
}
