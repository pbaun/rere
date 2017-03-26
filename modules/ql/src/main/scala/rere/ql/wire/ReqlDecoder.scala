package rere.ql.wire

import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.util.UUID

import akka.util.ByteString
import io.circe._
import rere.ql.data._
import rere.ql.shapes.ModelShape

import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * It should convert json to regular scala type
  */
trait ReqlDecoder[ScalaType] {

  def decode(json: Json): ReqlDecoder.Result[ScalaType]
}

case class DecodingError(message: String, json: Json)

object ReqlDecoder {
  // For capability with scala 2.11
  import cats.syntax.either._

  def apply[ScalaType](implicit decoder: ReqlDecoder[ScalaType]): ReqlDecoder[ScalaType] = decoder

  type Result[T] = Either[DecodingError, T]

  private def asReqlType(json: Json, tpe: String): Option[JsonObject] = {
    json.asObject match {
      case Some(obj) =>
        obj("$reql_type$") match {
          case Some(typeField) =>
            if (typeField.asString.contains(tpe)) Some(obj) else None

          case _ =>
            // no type field
            None
        }

      case _ =>
        // not object
        None
    }
  }

  implicit val booleanReqlDecoder: ReqlDecoder[Boolean] = {
    new ReqlDecoder[Boolean] {
      override def decode(json: Json): Result[Boolean] = {
        json.asBoolean match {
          case Some(boolean) => Right(boolean)
          case _ => Left(DecodingError("Not a boolean", json))
        }
      }
    }
  }

  implicit val intReqlDecoder: ReqlDecoder[Int] = {
    new ReqlDecoder[Int] {
      override def decode(json: Json): Result[Int] = {
        json.asNumber match {
          case Some(number) =>
            number.toInt match {
              case Some(intNumber) => Right(intNumber)
              case _ => Left(DecodingError("Not an int number", json))
            }

          case _ => Left(DecodingError("Not a number", json))
        }
      }
    }
  }

  implicit val longReqlDecoder: ReqlDecoder[Long] = {
    new ReqlDecoder[Long] {
      override def decode(json: Json): Result[Long] = {
        json.asNumber match {
          case Some(number) =>
            number.toLong match {
              case Some(longNumber) => Right(longNumber)
              case _ => Left(DecodingError("Not an long number", json))
            }

          case _ => Left(DecodingError("Not a number", json))
        }
      }
    }
  }

  implicit val bigIntReqlDecoder: ReqlDecoder[BigInt] = {
    new ReqlDecoder[BigInt] {
      override def decode(json: Json): Result[BigInt] = {
        json.asNumber match {
          case Some(number) =>
            number.toBigInt match {
              case Some(bigInt) => Right(bigInt)
              case _ => Left(DecodingError("Not an big int number", json))
            }

          case _ => Left(DecodingError("Not a number", json))
        }
      }
    }
  }

  implicit val doubleReqlDecoder: ReqlDecoder[Double] = {
    new ReqlDecoder[Double] {
      override def decode(json: Json): Result[Double] = {
        json.asNumber match {
          case Some(number) => Right(number.toDouble)                 //TODO: maybe need to check bounds?
          case _ => Left(DecodingError("Not a number", json))
        }
      }
    }
  }

  implicit val bigDecimalReqlDecoder: ReqlDecoder[BigDecimal] = {
    new ReqlDecoder[BigDecimal] {
      override def decode(json: Json): Result[BigDecimal] = {
        json.asNumber match {
          case Some(number) =>
            number.toBigDecimal match {
              case Some(bigDecimal) => Right(bigDecimal)
              case _ => Left(DecodingError("Not an big decimal number", json))
            }

          case _ => Left(DecodingError("Not a number", json))
        }
      }
    }
  }

  implicit val stringReqlDecoder: ReqlDecoder[String] = {
    new ReqlDecoder[String] {
      override def decode(json: Json): Result[String] = {
        json.asString match {
          case Some(string) => Right(string)
          case _ => Left(DecodingError("Not a string", json))
        }
      }
    }
  }

  implicit val zonedDataTimeReqlDecoder: ReqlDecoder[ZonedDateTime] = {
    /*
    * {
    *   "$reql_type$": "TIME",
    *   "epoch_time": 1482356816.89,
    *   "timezone": "+00:00"
    * }
    * */
    new ReqlDecoder[ZonedDateTime] {
      override def decode(json: Json): Result[ZonedDateTime] = {
        asReqlType(json, "TIME") match {
          case Some(obj) =>
            obj("epoch_time") match {
              case Some(epochTimeJson) =>
                epochTimeJson.asNumber match {
                  case Some(epochTime) =>
                    epochTime.toBigDecimal match {
                      case Some(epochTimeBigDecimal) =>
                        obj("timezone") match {
                          case Some(timezoneJson) =>
                            timezoneJson.asString match {
                              case Some(timezone) =>
                                val epochSeconds = epochTimeBigDecimal.toLong
                                val nanos = (epochTimeBigDecimal.remainder(1) * 1e9).toLong
                                val instant = Instant.ofEpochSecond(epochSeconds, nanos)
                                Right(ZonedDateTime.ofInstant(instant, ZoneOffset.of(timezone)))

                              case _ =>
                                Left(DecodingError("Field 'timezone' is not a string", timezoneJson))
                            }

                          case _ =>
                            Left(DecodingError("Field 'timezone' is missing", json))
                        }
                      case _ =>
                        Left(DecodingError("Not a big decimal", epochTimeJson))
                    }

                  case _ =>
                    Left(DecodingError("Field 'epoch_time' is not a number", epochTimeJson))
                }

              case _ =>
                Left(DecodingError("Field 'epoch_time' is missing", json))
            }

          case _ =>
            Left(DecodingError("Not 'TIME' pseudotype", json))
        }
      }
    }
  }

  implicit val uuidReqlDecoder: ReqlDecoder[UUID] = {
    new ReqlDecoder[UUID] {
      override def decode(json: Json): Result[UUID] = {
        json.asString match {
          case Some(string) =>
            //TODO: better validation?
            try {
              Right(UUID.fromString(string))
            } catch {
              case NonFatal(ex) =>
                Left(DecodingError(s"Error during uuid parsing: $ex", json))
            }
          case _ => Left(DecodingError("Not a string", json))
        }
      }
    }
  }

  implicit val jsonReqlDecoder: ReqlDecoder[Json] = {
    new ReqlDecoder[Json] {
      override def decode(json: Json): Result[Json] = {
        Right(json)
      }
    }
  }

  implicit val jsonObjectReqlDecoder: ReqlDecoder[JsonObject] = {
    new ReqlDecoder[JsonObject] {
      override def decode(json: Json): Result[JsonObject] = {
        json.asObject match {
          case Some(obj) => Right(obj)
          case _ => Left(DecodingError("Not an object", json))
        }
      }
    }
  }

  implicit val byteStringReqlDecoder: ReqlDecoder[ByteString] = {
    val base64Decoder = java.util.Base64.getDecoder
    /*
    * {
    *   "$reql_type$": "BINARY",
    *   "data": "dW5pY29kZSBzdHJpbmcgz4A="
    * }
    * */
    new ReqlDecoder[ByteString] {
      override def decode(json: Json): Result[ByteString] = {
        asReqlType(json, "BINARY") match {
          case Some(obj) =>
            obj("data") match {
              case Some(data) =>
                data.asString match {
                  case Some(dataString) =>
                    try {
                      Right(ByteString(base64Decoder.decode(dataString)))
                    } catch {
                      case NonFatal(ex) =>
                        Left(DecodingError(s"Error during base64 parsing: $ex", json))
                    }

                  case _ =>
                    Left(DecodingError("Field 'data' is not a string", data))
                }

              case _ =>
                Left(DecodingError("Field 'data' is missing", json))
            }

          case _ =>
            Left(DecodingError("Not 'BINARY' pseudotype", json))
        }
      }
    }
  }

  implicit def optionReqlDecoder[T](implicit innerDecoder: ReqlDecoder[T]): ReqlDecoder[Option[T]] = {
    new ReqlDecoder[Option[T]] {
      override def decode(json: Json): Result[Option[T]] = {
        if (json.isNull) {
          Right(None)
        } else {
          innerDecoder.decode(json).map(Some(_))
        }
      }
    }
  }

  implicit def seqReqlDecoder[T](implicit innerDecoder: ReqlDecoder[T]): ReqlDecoder[Seq[T]] = {
    new ReqlDecoder[Seq[T]] {
      override def decode(json: Json): Result[Seq[T]] = {
        json.asArray match {
          case Some(array) =>
            val decoded = new mutable.ListBuffer[T]()
            val errors = new mutable.ListBuffer[DecodingError]()

            array.foreach { elem =>
              innerDecoder.decode(elem) match {
                case Right(el) => decoded += el
                case Left(err) => errors += err
              }
            }

            if (errors.isEmpty) {
              Right(decoded)
            } else {
              Left(DecodingError(s"Error during array decoding: $errors", json))
            }

          case _ => Left(DecodingError("Not an array", json))
        }
      }
    }
  }

  implicit def modelReqlDecoder[M, PK](implicit modelShape: ModelShape[M, PK]): ReqlDecoder[M] = {
    new ReqlDecoder[M] {
      override def decode(json: Json): Result[M] = {
        modelShape.fromJson(json) match {
          case Right(model) => Right(model)
          case Left(err) => Left(DecodingError(s"Error during model decoding: $err", json))
        }
      }
    }
  }

  implicit def changefeedNotificationReqlDecoder[T](
                                                     implicit innerReqlDecoder: ReqlDecoder[Option[T]]
  ): ReqlDecoder[ChangefeedNotification[T]] = {
    new ReqlDecoder[ChangefeedNotification[T]] {
      override def decode(json: Json): Result[ChangefeedNotification[T]] = {
        json.asObject match {
          case Some(jsonObject) =>
            val oldVal = jsonObject("old_val") match {
              case Some(oldValJson) => innerReqlDecoder.decode(oldValJson)
              case _ => Left(DecodingError("Field 'old_val' is missing", json))
            }

            val newVal = jsonObject("new_val") match {
              case Some(newValJson) => innerReqlDecoder.decode(newValJson)
              case _ => Left(DecodingError("Field 'new_val' is missing", json))
            }

            (oldVal, newVal) match {
              case (Right(oldV), Right(newV)) => Right(ChangefeedNotification(oldV, newV))
              case (Left(oldErr), Left(newErr)) => Left(DecodingError(s"Can't decode changefeed notification: $oldErr, $newErr", json))
              case (Left(oldErr), _) => Left(DecodingError(s"Can't decode changefeed notification: $oldErr", json))
              case (_, Left(newErr)) => Left(DecodingError(s"Can't decode changefeed notification: $newErr", json))
            }

          case _ =>
            Left(DecodingError("Not an object", json))
        }
      }
    }
  }

  private implicit def changefeedNotificationDecoder[T](
    implicit innerReqlDecoder: ReqlDecoder[Option[T]]
  ): Decoder[ChangefeedNotification[T]] = {
    circeFromReql(changefeedNotificationReqlDecoder[T])
  }

  implicit def optionListChangefeedNotificationTDecoder[T](
    implicit innerDecoder: Decoder[ChangefeedNotification[T]]
  ): Decoder[Option[List[ChangefeedNotification[T]]]] = {
    Decoder.decodeOption(
      Decoder.decodeList[ChangefeedNotification[T]]
    )
  }

  private implicit def insertionResultDecoder[T : ReqlDecoder, PK : ReqlDecoder]: Decoder[ModificationResult[T, PK]] = {
    implicit val pkDecoder: Decoder[PK] = circeFromReql(ReqlDecoder[PK])
    Decoder.instance(c =>
      for {
        inserted <- c.downField("inserted").as[Long]
        replaced <- c.downField("replaced").as[Long]
        unchanged <- c.downField("unchanged").as[Long]
        errors <- c.downField("errors").as[Long]
        firstError <- c.downField("first_error").as[Option[String]]
        deleted <- c.downField("deleted").as[Long]
        skipped <- c.downField("skipped").as[Long]
        generatedKeys <- c.downField("generated_keys").as[Option[List[PK]]]
        warnings <- c.downField("warnings").as[Option[String]]
        changes <- c.downField("changes").as[Option[List[ChangefeedNotification[T]]]]
      } yield ModificationResult[T, PK](inserted, replaced, unchanged, errors, firstError, deleted, skipped, generatedKeys, warnings, changes)
    )
  }

  implicit def insertionResultReqlDecoder[T : ReqlDecoder, PK : ReqlDecoder]: ReqlDecoder[ModificationResult[T, PK]] = {
    reqlFromCirce[ModificationResult[T, PK]](insertionResultDecoder)
  }

  private implicit val databaseConfigDecoder: Decoder[DatabaseConfig] = Decoder.instance(c =>
    for {
      id <- c.downField("id").as[UUID]
      name <- c.downField("name").as[String]
    } yield DatabaseConfig(id, name)
  )

  implicit val databaseConfigReqlDecoder: ReqlDecoder[DatabaseConfig] = {
    reqlFromCirce[DatabaseConfig](databaseConfigDecoder)
  }

  private implicit val databaseCreationResultDecoder: Decoder[DatabaseCreationResult] = Decoder.instance(c =>
    for {
      dbsCreated <- c.downField("dbs_created").as[Long]
      configChanges <- c.downField("config_changes").as[List[ChangefeedNotification[DatabaseConfig]]]
    } yield DatabaseCreationResult(dbsCreated, configChanges)
  )

  implicit val databaseCreationResultReqlDecoder: ReqlDecoder[DatabaseCreationResult] = {
    reqlFromCirce[DatabaseCreationResult](databaseCreationResultDecoder)
  }

  private implicit val databaseDroppingResultDecoder: Decoder[DatabaseDroppingResult] = Decoder.instance(c =>
    for {
      dbsDropped <- c.downField("dbs_dropped").as[Long]
      tablesDropped <- c.downField("tables_dropped").as[Long]
      configChanges <- c.downField("config_changes").as[List[ChangefeedNotification[DatabaseConfig]]]
    } yield DatabaseDroppingResult(dbsDropped, tablesDropped, configChanges)
  )

  implicit val databaseDroppingResultReqlDecoder: ReqlDecoder[DatabaseDroppingResult] = {
    reqlFromCirce[DatabaseDroppingResult](databaseDroppingResultDecoder)
  }

  private implicit val shardDecoder: Decoder[Shard] = Decoder.instance(c =>
    for {
      primaryReplica <- c.downField("primary_replica").as[String]
      replicas <- c.downField("replicas").as[List[String]]
      nonvotingReplicas <- c.downField("nonvoting_replicas").as[List[String]]
    } yield Shard(primaryReplica = primaryReplica, replicas = replicas, nonvotingReplicas = nonvotingReplicas)
  )

  private implicit val tableConfigDecoder: Decoder[TableConfig] = Decoder.instance(c =>
    for {
      id <- c.downField("id").as[UUID]
      name <- c.downField("name").as[String]
      db <- c.downField("db").as[String]
      primaryKey <- c.downField("primary_key").as[String]
      shards <- c.downField("shards").as[List[Shard]]
      indexes <- c.downField("indexes").as[List[String]]
      writeAcks <- c.downField("write_acks").as[String]
      durability <- c.downField("durability").as[String]
    } yield TableConfig(
      id = id,
      name = name,
      db = db,
      primaryKey = primaryKey,
      shards = shards,
      indexes = indexes,
      writeAcks = writeAcks,
      durability = durability
    )
  )

  implicit val tableConfigReqlDecoder: ReqlDecoder[TableConfig] = {
    reqlFromCirce[TableConfig](tableConfigDecoder)
  }

  private implicit val tableCreationResultDecoder: Decoder[TableCreationResult] = Decoder.instance(c =>
    for {
      tablesCreated <- c.downField("tables_created").as[Long]
      configChanges <- c.downField("config_changes").as[List[ChangefeedNotification[TableConfig]]]
    } yield TableCreationResult(tablesCreated, configChanges)
  )

  implicit val tableCreationResultReqlDecoder: ReqlDecoder[TableCreationResult] = {
    reqlFromCirce[TableCreationResult](tableCreationResultDecoder)
  }

  private implicit val tableDroppingResultDecoder: Decoder[TableDroppingResult] = Decoder.instance(c =>
    for {
      tablesDropped <- c.downField("tables_dropped").as[Long]
      configChanges <- c.downField("config_changes").as[List[ChangefeedNotification[TableConfig]]]
    } yield TableDroppingResult(tablesDropped, configChanges)
  )

  implicit val tableDroppingResultReqlDecoder: ReqlDecoder[TableDroppingResult] = {
    reqlFromCirce[TableDroppingResult](tableDroppingResultDecoder)
  }

  private implicit val indexCreationResultDecoder: Decoder[IndexCreationResult] = Decoder.instance(c =>
    for {
      created <- c.downField("created").as[Long]
    } yield IndexCreationResult(created)
  )

  implicit val indexCreationResultReqlDecoder: ReqlDecoder[IndexCreationResult] = {
    reqlFromCirce[IndexCreationResult](indexCreationResultDecoder)
  }

  private implicit val indexDroppingResultDecoder: Decoder[IndexDroppingResult] = Decoder.instance(c =>
    for {
      dropped <- c.downField("dropped").as[Long]
    } yield IndexDroppingResult(dropped)
  )

  implicit val indexDroppingResultReqlDecoder: ReqlDecoder[IndexDroppingResult] = {
    reqlFromCirce[IndexDroppingResult](indexDroppingResultDecoder)
  }

  private implicit val indexRenamingResultDecoder: Decoder[IndexRenamingResult] = Decoder.instance(c =>
    for {
      renamed <- c.downField("renamed").as[Long]
    } yield IndexRenamingResult(renamed)
  )

  implicit val indexRenamingResultReqlDecoder: ReqlDecoder[IndexRenamingResult] = {
    reqlFromCirce[IndexRenamingResult](indexRenamingResultDecoder)
  }

  implicit val indexFunctionBinaryReqlDecoder: ReqlDecoder[IndexFunctionBinary] = {
    new ReqlDecoder[IndexFunctionBinary] {
      override def decode(json: Json): Result[IndexFunctionBinary] = {
        asReqlType(json, "BINARY") match {
          case Some(obj) =>
            obj("data") match {
              case Some(data) =>
                data.asString match {
                  case Some(dataString) =>
                    Right(IndexFunctionBinary(dataString))

                  case _ =>
                    Left(DecodingError("Field 'data' is not a string", data))
                }

              case _ =>
                Left(DecodingError("Field 'data' is missing", json))
            }

          case _ =>
            Left(DecodingError("Not 'BINARY' pseudotype", json))
        }
      }
    }
  }

  private implicit val indexFunctionBinaryDecoder: Decoder[IndexFunctionBinary] = {
    circeFromReql(indexFunctionBinaryReqlDecoder)
  }

  private implicit val indexStatusDecoder: Decoder[IndexStatus] = Decoder.instance(c =>
    for {
      index <- c.downField("index").as[String]
      ready <- c.downField("ready").as[Boolean]
      progress <- c.downField("progress").as[Option[BigDecimal]]
      function <- c.downField("function").as[IndexFunctionBinary]
      multi <- c.downField("multi").as[Boolean]
      geo <- c.downField("geo").as[Boolean]
      outdated <- c.downField("outdated").as[Boolean]
    } yield IndexStatus(index, ready, progress, function, multi, geo, outdated)
  )

  implicit val indexStatusReqlDecoder: ReqlDecoder[IndexStatus] = {
    reqlFromCirce[IndexStatus](indexStatusDecoder)
  }

  private implicit val tableReplicaStatusDecoder: Decoder[TableReplicaStatus] = Decoder.instance(c =>
    for {
      server <- c.downField("server").as[String]
      state <- c.downField("state").as[String]
    } yield TableReplicaStatus(server, state)
  )

  private implicit val tableShardStatusDecoder: Decoder[TableShardStatus] = Decoder.instance(c =>
    for {
      primaryReplicas <- c.downField("primary_replicas").as[List[String]]
      replicas <- c.downField("replicas").as[List[TableReplicaStatus]]
    } yield TableShardStatus(primaryReplicas, replicas)
  )

  private implicit val tableStatusFlagsDecoder: Decoder[TableStatusFlags] = Decoder.instance(c =>
    for {
      readyForOutdatedReads <- c.downField("ready_for_outdated_reads").as[Boolean]
      readyForReads <- c.downField("ready_for_reads").as[Boolean]
      readyForWrites <- c.downField("ready_for_writes").as[Boolean]
      allReplicasReady <- c.downField("all_replicas_ready").as[Boolean]
    } yield TableStatusFlags(readyForOutdatedReads, readyForReads, readyForWrites, allReplicasReady)
  )

  private implicit val tableStatusDecoder: Decoder[TableStatus] = Decoder.instance(c =>
    for {
      id <- c.downField("id").as[UUID]
      name <- c.downField("name").as[String]
      db <- c.downField("db").as[String]
      status <- c.downField("status").as[TableStatusFlags]
      shards <- c.downField("shards").as[List[TableShardStatus]]
      raftLeader <- c.downField("raft_leader").as[String]
    } yield TableStatus(id, name, db, status, shards, raftLeader)
  )

  implicit val tableStatusReqlDecoder: ReqlDecoder[TableStatus] = {
    reqlFromCirce[TableStatus](tableStatusDecoder)
  }

  private implicit val rebalancingResultDecoder: Decoder[RebalancingResult] = Decoder.instance(c =>
    for {
      rebalanced <- c.downField("rebalanced").as[Long]
      statusChanges <- c.downField("status_changes").as[Seq[ChangefeedNotification[TableStatus]]]
    } yield RebalancingResult(rebalanced, statusChanges)
  )

  implicit val rebalancingResultReqlDecoder: ReqlDecoder[RebalancingResult] = {
    reqlFromCirce[RebalancingResult](rebalancingResultDecoder)
  }

  private implicit val reconfiguringResultDecoder: Decoder[ReconfiguringResult] = Decoder.instance(c =>
    for {
      reconfigured <- c.downField("reconfigured").as[Long]
      configChanges <- c.downField("config_changes").as[Seq[ChangefeedNotification[TableConfig]]]
      statusChanges <- c.downField("status_changes").as[Seq[ChangefeedNotification[TableStatus]]]
    } yield ReconfiguringResult(reconfigured, configChanges, statusChanges)
  )

  implicit val reconfiguringResultReqlDecoder: ReqlDecoder[ReconfiguringResult] = {
    reqlFromCirce[ReconfiguringResult](reconfiguringResultDecoder)
  }

  private implicit val reconfiguringDryResultDecoder: Decoder[ReconfiguringDryResult] = Decoder.instance(c =>
    for {
      reconfigured <- c.downField("reconfigured").as[Long]
      configChanges <- c.downField("config_changes").as[Seq[ChangefeedNotification[TableConfig]]]
    } yield ReconfiguringDryResult(reconfigured, configChanges)
  )

  implicit val reconfiguringDryResultReqlDecoder: ReqlDecoder[ReconfiguringDryResult] = {
    reqlFromCirce[ReconfiguringDryResult](reconfiguringDryResultDecoder)
  }

  private implicit val waitingResultDecoder: Decoder[WaitingResult] = Decoder.instance(c =>
    for {
      ready <- c.downField("ready").as[Long]
    } yield WaitingResult(ready)
  )

  implicit val waitingResultReqlDecoder: ReqlDecoder[WaitingResult] = {
    reqlFromCirce[WaitingResult](waitingResultDecoder)
  }

  private implicit val userPermissionsDecoder: Decoder[UserPermissions] = Decoder.instance(c =>
    for {
      read <- c.downField("read").as[Option[Boolean]]
      write <- c.downField("write").as[Option[Boolean]]
      connect <- c.downField("connect").as[Option[Boolean]]
      config <- c.downField("config").as[Option[Boolean]]
    } yield UserPermissions(read, write, connect, config)
  )

  implicit val userPermissionsReqlDecoder: ReqlDecoder[UserPermissions] = {
    reqlFromCirce[UserPermissions](userPermissionsDecoder)
  }

  private implicit val grantingResultDecoder: Decoder[GrantingResult] = Decoder.instance(c =>
    for {
      granted <- c.downField("granted").as[Long]
      permissionsChanges <- c.downField("permissions_changes").as[Seq[ChangefeedNotification[UserPermissions]]]
    } yield GrantingResult(granted, permissionsChanges)
  )

  implicit val grantingResultReqlDecoder: ReqlDecoder[GrantingResult] = {
    reqlFromCirce[GrantingResult](grantingResultDecoder)
  }

  private implicit val syncingResultDecoder: Decoder[SyncingResult] = Decoder.instance(c =>
    for {
      synced <- c.downField("synced").as[Long]
    } yield SyncingResult(synced)
  )

  implicit val syncingResultReqlDecoder: ReqlDecoder[SyncingResult] = {
    reqlFromCirce[SyncingResult](syncingResultDecoder)
  }

  private implicit def distanceResultDecoder[T : ReqlDecoder]: Decoder[DistanceResult[T]] = Decoder.instance(c =>
    for {
      dist <- c.downField("dist").as[BigDecimal]
      doc <- c.downField("doc").as[T](circeFromReql(ReqlDecoder[T]))
    } yield DistanceResult(dist, doc)
  )

  implicit def distanceResultReqlDecoder[T : ReqlDecoder]: ReqlDecoder[DistanceResult[T]] = {
    reqlFromCirce(distanceResultDecoder)
  }

  private val coordinatesPairReqlDecoder: ReqlDecoder[GeoPoint] = {
    //[123,22]
    new ReqlDecoder[GeoPoint] {
      override def decode(coordinatesPair: Json): Result[GeoPoint] = {
        coordinatesPair.asArray match {
          case Some(coordinatesArray) =>
            coordinatesArray match {
              case Vector(lng, lat) =>
                lng.asNumber match {
                  case Some(longitude) =>
                    val lngDouble = longitude.toDouble
                    if (-180 <= lngDouble && lngDouble <= 180) {
                      lat.asNumber match {
                        case Some(latitude) =>
                          val latDouble = latitude.toDouble
                          if (-90 <= latDouble && latDouble <= 90) {
                            Right(GeoPoint(lngDouble, latDouble))
                          } else {
                            Left(DecodingError("Second element of coordinates pair is not in [-90, 90] interval", lat))
                          }

                        case _ =>
                          Left(DecodingError("Second element of coordinates pair is not a number", lat))
                      }
                    } else {
                      Left(DecodingError("First element of coordinates pair is not in [-180, 180] interval", lng))
                    }

                  case _ =>
                    Left(DecodingError("First element of coordinates pair is not a number", lng))
                }

              case _ =>
                Left(DecodingError("Coordinates pair should have 2 elements", coordinatesPair))
            }

          case _ =>
            Left(DecodingError("Coordinates pair should be an array", coordinatesPair))
        }
      }
    }
  }

  private val seqOfCoordinatesPairReqlDecoder: ReqlDecoder[Seq[GeoPoint]] = {
    val coordinatesPairDecoder: Decoder[GeoPoint] = {
      circeFromReql(coordinatesPairReqlDecoder)
    }

    val seqOfCoordinatesPairDecoder: Decoder[Seq[GeoPoint]] = Decoder.instance { c =>
      c.as[Seq[GeoPoint]](Decoder.decodeCanBuildFrom[GeoPoint, Seq](
        coordinatesPairDecoder,
        Seq.canBuildFrom
      ))
    }

    reqlFromCirce(seqOfCoordinatesPairDecoder)
  }

  implicit val geoPointReqlDecoder: ReqlDecoder[GeoPoint] = {
    /*
    * {"$reql_type$":"GEOMETRY","coordinates":[123,22],"type":"Point"}
    * */
    new ReqlDecoder[GeoPoint] {
      override def decode(json: Json): Result[GeoPoint] = {
        asReqlType(json, "GEOMETRY") match {
          case Some(obj) =>
            obj("type") match {
              case Some(geometryType) =>
                geometryType.asString match {
                  case Some("Point") =>
                    obj("coordinates") match {
                      case Some(coordinatesPair) =>
                        coordinatesPairReqlDecoder.decode(coordinatesPair)

                      case _ =>
                        Left(DecodingError("Field 'coordinates' is missing", json))
                    }

                  case _ =>
                    Left(DecodingError("Field 'type' is not a 'Point'", geometryType))
                }

              case _ =>
                Left(DecodingError("Field 'type' is missing", json))
            }

          case _ =>
            Left(DecodingError("Not 'GEOMETRY' pseudotype", json))
        }
      }
    }
  }

  private implicit val geoPointDecoder: Decoder[GeoPoint] = {
    circeFromReql(geoPointReqlDecoder)
  }

  implicit val geoLineStringReqlDecoder: ReqlDecoder[GeoLineString] = {
    //{"$reql_type$":"GEOMETRY","coordinates":[[123,22],[124,23]],"type":"LineString"}
    //{"$reql_type$":"GEOMETRY","coordinates":[[123,22],[124,23],[125,24]],"type":"LineString"}
    new ReqlDecoder[GeoLineString] {
      override def decode(json: Json): Result[GeoLineString] = {
        asReqlType(json, "GEOMETRY") match {
          case Some(obj) =>
            obj("type") match {
              case Some(geometryType) =>
                geometryType.asString match {
                  case Some("LineString") =>
                    obj("coordinates") match {
                      case Some(coordinates) =>
                        seqOfCoordinatesPairReqlDecoder.decode(coordinates) match {
                          case Right(points) =>
                            points match {
                              case Seq(point1, point2, rest @ _ *) =>
                                Right(GeoLineString(point1, point2, rest: _*))

                              case _ =>
                                Left(DecodingError("Field 'coordinates' should contain at least 2 coordinate pairs", json))
                            }

                          case Left(err) =>
                            Left(err)
                        }

                      case _ =>
                        Left(DecodingError("Field 'coordinates' is missing", json))
                    }

                  case _ =>
                    Left(DecodingError("Field 'type' is not a 'LineString'", geometryType))
                }

              case _ =>
                Left(DecodingError("Field 'type' is missing", json))
            }

          case _ =>
            Left(DecodingError("Not 'GEOMETRY' pseudotype", json))
        }
      }
    }
  }

  private val seqOfGeoLinearRingReqlDecoder: ReqlDecoder[Seq[GeoLinearRing]] = {
    val geoLinearRingDecoder: Decoder[GeoLinearRing] = {
      val geoLinearRingReqlDecoder: ReqlDecoder[GeoLinearRing] = {
        new ReqlDecoder[GeoLinearRing] {
          override def decode(coordinates: Json): Result[GeoLinearRing] = {
            seqOfCoordinatesPairReqlDecoder.decode(coordinates) match {
              case Right(points) =>
                points match {
                  case Seq(point1, point2, point3, rest @ _ *) =>
                    Right(GeoLinearRing(point1, point2, point3, rest: _*))

                  case _ =>
                    Left(DecodingError("Linear ring should contain at least 3 coordinate pairs", coordinates))
                }

              case Left(err) =>
                Left(err)
            }
          }
        }
      }

      circeFromReql(geoLinearRingReqlDecoder)
    }

    val seqOfGeoLinearRingDecoder: Decoder[Seq[GeoLinearRing]] = Decoder.instance { c =>
      c.as[Seq[GeoLinearRing]](Decoder.decodeCanBuildFrom[GeoLinearRing, Seq](
        geoLinearRingDecoder,
        Seq.canBuildFrom
      ))
    }

    reqlFromCirce(seqOfGeoLinearRingDecoder)
  }

  implicit val geoPolygonReqlDecoder: ReqlDecoder[GeoPolygon] = {
    //{"$reql_type$":"GEOMETRY","coordinates":[[[123,22],[124,23],[124,25],[123,22]]],"type":"Polygon"}
    new ReqlDecoder[GeoPolygon] {
      override def decode(json: Json): Result[GeoPolygon] = {
        asReqlType(json, "GEOMETRY") match {
          case Some(obj) =>
            obj("type") match {
              case Some(geometryType) =>
                geometryType.asString match {
                  case Some("Polygon") =>
                    obj("coordinates") match {
                      case Some(coordinates) =>
                        seqOfGeoLinearRingReqlDecoder.decode(coordinates) match {
                          case Right(points) =>
                            points match {
                              case Seq(exteriorRing, interiorRings @ _ *) =>
                                Right(GeoPolygon(exteriorRing, interiorRings: _*))

                              case _ =>
                                Left(DecodingError("Field 'coordinates' should contain at least 1 linear ring", json))
                            }

                          case Left(err) =>
                            Left(err)
                        }

                      case _ =>
                        Left(DecodingError("Field 'coordinates' is missing", json))
                    }

                  case _ =>
                    Left(DecodingError("Field 'type' is not a 'Polygon'", geometryType))
                }

              case _ =>
                Left(DecodingError("Field 'type' is missing", json))
            }

          case _ =>
            Left(DecodingError("Not 'GEOMETRY' pseudotype", json))
        }
      }
    }
  }

  /**
    * Universal ReqlDecoder from circe Decoder
    * @param decoder - instance of io.circe.Decoder
    * @tparam T - type for which ReqlDecoder will be crafted
    * @return
    */
  private def reqlFromCirce[T](decoder: Decoder[T]): ReqlDecoder[T] = {
    new ReqlDecoder[T] {
      override def decode(json: Json): Result[T] = {
        decoder.decodeJson(json) match {
          case Right(r) => Right(r)
          case Left(err) => Left(DecodingError(err.message, json))
        }
      }
    }
  }

  private def circeFromReql[T](reqlDecoder: ReqlDecoder[T]): Decoder[T] = {
    Decoder.instance(c =>
      reqlDecoder.decode(c.value) match {
        case Right(r) => Right(r)
        case Left(err) => Left(DecodingFailure(err.message, Nil))
      }
    )
  }
}
