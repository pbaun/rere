package rere.driver.auth

import java.nio.ByteOrder
import java.nio.charset.Charset

import akka.NotUsed
import akka.event.LoggingAdapter
import akka.stream.scaladsl.{Flow, Framing, GraphDSL}
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.stream.{Attributes, FlowShape, Inlet, Outlet}
import akka.util.ByteString
import cats.syntax.either._
import io.circe.syntax._
import rere.driver.exceptions.{ReqlAuthError, ReqlDriverError}
import rere.ql.ql2
import rere.sasl.gs2.ChannelBindingFlag
import rere.sasl.scram.client._
import rere.sasl.scram.parsers.SCRAMParser
import rere.sasl.scram.rendering.SCRAMRenderer

class AuthCommander(
    scramClient: ClientFirstStep,
    login: String,
    password: String,
    logger: LoggingAdapter)
  extends GraphStage[FlowShape[ByteString, ByteString]] {

  val fromServer = Inlet[ByteString]("Commander.fromServer")
  val toServer = Outlet[ByteString]("Commander.toServer")

  override val shape = FlowShape.of(fromServer, toServer)

  implicit private val BYTE_ORDER = ByteOrder.LITTLE_ENDIAN
  private val CHARSET = Charset.forName("UTF-8")
  private val PROTOCOL_SUB_VERSION = 0
  private val AUTHENTICATION_METHOD = "SCRAM-SHA-256"

  private val INITIAL = 0
  private val FIRST_MESSAGE_SENT = 1
  private val PROTOCOL_VERSION_CHECKED = 2
  private val SECOND_MESSAGE_SENT = 3

  import io.circe.generic.auto._
  import io.circe.parser._

  private case class GeneralServerMessage(
    success: Boolean,
    error_code: Option[Int],
    error: Option[String])
  private case class FirstClientAuthMessage(
    protocol_version: Int,
    authentication_method: String,
    authentication: String)
  private case class ProtocolSubversionMessage(
    max_protocol_version: Int,
    min_protocol_version: Int,
    server_version: String)
  private case class FirstServerAuthMessage(authentication: String)
  private case class FinalClientAuthMessage(authentication: String)
  private case class FinalServerAuthMessage(authentication: String)

  private def handleHandshakeError(errorCode: Option[Int], errorMessage: Option[String]): Exception = {
    errorCode match {
      case Some(code) if 10 <= code && code <= 20 =>
        // it is shortcut for scram errors, server does not wait for final message
        // more details in authentication_error.hpp and scram_authenticator.cc
        new ReqlAuthError(errorMessage.getOrElse(s"<no error message, code = $code>"))

      case Some(code) =>
        new ReqlDriverError(errorMessage.getOrElse(s"<no error message, code = $code>"))

      case _ =>
        new ReqlDriverError(errorMessage.getOrElse("<no error message, no error code>"))
    }
  }

  override def createLogic(att: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) {

      var stage = INITIAL
      var secondStepClient: ClientSecondStep = _
      var finalStepClient: ClientFinalStep = _

      setHandler(fromServer, new InHandler {
        private def handleServerSentError(msg: String, nextHandler: String => Unit): Unit = {
          val successOrErrorXor = decode[GeneralServerMessage](msg)
          logger.debug("Server sent error: {}", successOrErrorXor)

          successOrErrorXor match {
            case Right(successOrError) =>
              if (successOrError.success) {
                nextHandler(msg)
              } else {
                val exception = handleHandshakeError(successOrError.error_code, successOrError.error)
                fail(toServer, exception)
              }

            case Left(parsingError) =>
              fail(toServer, new ReqlProtocolParsingError(
                s"Can't parse message '$msg'", parsingError
              ))
          }
        }

        override def onPush(): Unit = {
          logger.debug("onPush [{}]", stage)
          //answers from server
          //push(toServer, grab(fromServer))
          stage match {
            case INITIAL =>
              //server pushed before we asked him. just ignore

            case FIRST_MESSAGE_SENT =>
              handleServerSentError(grab(fromServer).utf8String, { msgString =>
                decode[ProtocolSubversionMessage](msgString) match {
                  case Right(msg) =>
                    if (PROTOCOL_SUB_VERSION <= msg.min_protocol_version &&
                      msg.max_protocol_version <= PROTOCOL_SUB_VERSION) {

                      stage = PROTOCOL_VERSION_CHECKED
                      pull(fromServer)
                    } else {
                      fail(toServer, new ReqlDriverError(
                        s"Unsupported protocol version $PROTOCOL_SUB_VERSION, " +
                          s"expected between ${msg.min_protocol_version} and ${msg.max_protocol_version}."
                      ))
                    }

                  case Left(error) =>
                    fail(toServer, new ReqlProtocolParsingError(
                      "Can't parse message", error
                    ))
                }
              })

            case PROTOCOL_VERSION_CHECKED =>
              handleServerSentError(grab(fromServer).utf8String, { msgString =>
                decode[FirstServerAuthMessage](msgString) match {
                  case Right(authMessage) =>
                    logger.debug("Auth message: {}", authMessage)

                    val parser = new SCRAMParser(authMessage.authentication)
                    Either.fromTry(parser.`server-first-message`.run()) match {
                      case Right(serverFirstMessage) =>

                        logger.debug("before processing")
                        val processingResult = secondStepClient.process(serverFirstMessage)
                        logger.debug("after processing")

                        processingResult match {
                          case Right(nextStepClient) =>

                            val message = FinalClientAuthMessage(SCRAMRenderer.renderToString(nextStepClient.finalMessage))
                            val toSent = ByteString(message.asJson.noSpaces.getBytes(CHARSET))
                            push(toServer, toSent)

                            finalStepClient = nextStepClient
                            stage = SECOND_MESSAGE_SENT

                          case Left(authError) =>
                            fail(toServer, new ReqlAuthError(
                              s"Auth failed after processing first server message. ${authError.msg}"
                            ))
                        }

                      case Left(scramParsingError) =>
                        fail(toServer, new ReqlProtocolParsingError(
                          "Can't parse scram message", scramParsingError
                        ))
                    }

                  case Left(error) =>
                    fail(toServer, new ReqlProtocolParsingError(
                      "Can't parse message", error
                    ))
                }
              })

            case SECOND_MESSAGE_SENT =>
              handleServerSentError(grab(fromServer).utf8String, { msgString =>
                decode[FinalServerAuthMessage](msgString) match {
                  case Right(authMessage) =>
                    logger.debug("Auth message: {}", authMessage)

                    val parser = new SCRAMParser(authMessage.authentication)
                    Either.fromTry(parser.`server-final-message`.run()) match {
                      case Right(serverFinalMessage) =>

                        logger.debug("before processing")
                        val processingResult = finalStepClient.process(serverFinalMessage)
                        logger.debug("after processing")

                        processingResult match {
                          case Right(authStatus) =>
                            logger.debug(s"Auth done: {}", authStatus)
                            complete(toServer)

                          case Left(authError) =>
                            fail(toServer, new ReqlAuthError(
                              s"Auth failed after processing final server message. ${authError.msg}"
                            ))
                        }

                      case Left(scramParsingError) =>
                        fail(toServer, new ReqlProtocolParsingError(
                          "Can't parse scram message", scramParsingError
                        ))
                    }

                  case Left(error) =>
                    fail(toServer, new ReqlProtocolParsingError(
                      "Can't parse message", error
                    ))
                }
              })
          }
        }

        override def onUpstreamFinish(): Unit = {
          logger.debug("auth from server finish")
          super.onUpstreamFinish()
        }
      })

      setHandler(toServer, new OutHandler {
        override def onPull(): Unit = {
          logger.debug("onPull [{}]", stage)

          stage match {
            case INITIAL =>
              val builder = ByteString.newBuilder
              builder.putInt(ql2.VersionDummy.Version.V1_0)

              val nextClient = scramClient.auth(login, password, ChannelBindingFlag.NotSupports, None, Nil)

              val message = FirstClientAuthMessage(
                PROTOCOL_SUB_VERSION,
                AUTHENTICATION_METHOD,
                SCRAMRenderer.renderToString(nextClient.firstMessage)
              )

              builder.putBytes(message.asJson.noSpaces.getBytes(CHARSET))
              val firstMessage = builder.result()

              push(toServer, firstMessage)
              secondStepClient = nextClient
              stage = FIRST_MESSAGE_SENT

            case FIRST_MESSAGE_SENT =>
              pull(fromServer)

            case PROTOCOL_VERSION_CHECKED =>
              // ignore

            case SECOND_MESSAGE_SENT =>
              pull(fromServer)
          }
        }
      })
    }
}

object AuthCommander {
  def getCommanderFlow(authCommander: AuthCommander): Flow[ByteString, ByteString, NotUsed] = {
    Flow.fromGraph(
      GraphDSL.create(authCommander) { implicit builder =>
        commander =>

          import GraphDSL.Implicits._

          val commandEncoder = builder.add(Flow[ByteString].map(command => command ++ ByteString("\u0000")))
          val commandDecoder = builder.add(Framing.delimiter(ByteString("\u0000"), maximumFrameLength = 4096, allowTruncation = false))

          commander.out ~> commandEncoder.in
          commander.in  <~ commandDecoder.out

          FlowShape.of(commandDecoder.in, commandEncoder.out)
      }
    )
  }
}
