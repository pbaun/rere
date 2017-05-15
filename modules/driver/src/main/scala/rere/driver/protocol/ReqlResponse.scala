package rere.driver.protocol

import akka.util.ByteString
import io.circe._
import rere.ql.ql2.Response.{ErrorType, ResponseType}

final case class ReqlResponse(
  responseType: Int,
  data: Vector[Json],
  profile: Option[Vector[Json]],
  backtrace: Option[Json],
  errorType: Option[Json]
)

object ReqlResponse {
  // For capability with scala 2.11
  import cats.syntax.either._

  private val reqlResponseDecoder: Decoder[ReqlResponse] =
    Decoder.instance(c =>
      for {
        responseType <- c.downField("t").as[Int]
        data <- c.downField("r").as[Vector[Json]]
        profile <- c.downField("p").as[Option[Vector[Json]]]
        backtrace <- c.downField("b").as[Option[Json]]
        errorType <- c.downField("e").as[Option[Json]]
      } yield ReqlResponse(responseType, data, profile, backtrace, errorType)
    )

  def decode(data: ByteString): Either[io.circe.Error, ReqlResponse] = {
    parser.decode[ReqlResponse](data.utf8String)(reqlResponseDecoder)
  }

  def determineError(response: ReqlResponse): ReqlServerSentError = {
    response.responseType match {
      case ResponseType.CLIENT_ERROR | ResponseType.COMPILE_ERROR | ResponseType.RUNTIME_ERROR =>
        val errorType: ReqlServerSentErrorType = response.responseType match {
          case ResponseType.CLIENT_ERROR =>
            ReqlClientError

          case ResponseType.COMPILE_ERROR =>
            ReqlCompileError

          case ResponseType.RUNTIME_ERROR =>
            val errorCodeOpt = for {
              errorType <- response.errorType
              errorNumber <- errorType.asNumber
              errorIntCode <- errorNumber.toInt
            } yield errorIntCode

            errorCodeOpt.map {
              case ErrorType.INTERNAL => ReqlInternalError
              case ErrorType.RESOURCE_LIMIT => ReqlResourceLimitError
              case ErrorType.QUERY_LOGIC => ReqlQueryLogicError
              case ErrorType.NON_EXISTENCE => ReqlNonExistenceError
              case ErrorType.OP_FAILED => ReqlOpFailedError
              case ErrorType.OP_INDETERMINATE => ReqlOpIndeterminateError
              case ErrorType.USER => ReqlUserError
              case ErrorType.PERMISSION_ERROR => ReqlPermissionError
              case _ => ReqlRuntimeError
            }.getOrElse {
              ReqlRuntimeError
            }
        }
        val description = response.data.headOption.flatMap(_.asString)
        val backtrace = response.backtrace

        ReqlServerSentError(errorType, description, backtrace)

      case _ =>
        ReqlServerSentError(
          ReqlClientError,
          Some(s"Unknown response type ${response.responseType} encountered in a response."),
          response.backtrace
        )
    }
  }
}

sealed trait ReqlServerSentErrorType
case object ReqlClientError extends ReqlServerSentErrorType
case object ReqlCompileError extends ReqlServerSentErrorType
case object ReqlInternalError extends ReqlServerSentErrorType
case object ReqlResourceLimitError extends ReqlServerSentErrorType
case object ReqlQueryLogicError extends ReqlServerSentErrorType
case object ReqlNonExistenceError extends ReqlServerSentErrorType
case object ReqlOpFailedError extends ReqlServerSentErrorType
case object ReqlOpIndeterminateError extends ReqlServerSentErrorType
case object ReqlUserError extends ReqlServerSentErrorType
case object ReqlPermissionError extends ReqlServerSentErrorType
case object ReqlRuntimeError extends ReqlServerSentErrorType

case class ReqlServerSentError(
  errorType: ReqlServerSentErrorType,
  description: Option[String],
  backtrace: Option[Json]
)
