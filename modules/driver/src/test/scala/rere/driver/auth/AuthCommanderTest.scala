package rere.driver.auth

import akka.Done
import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.scaladsl.Keep
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.stream.testkit.{TestPublisher, TestSubscriber}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.testkit.TestKit
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.circe.{DecodingFailure, ParsingFailure}
import org.parboiled2.ParseError
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Inside, Matchers}
import rere.driver.exceptions.{ReqlAuthError, ReqlDriverError}
import rere.sasl.gs2
import rere.sasl.gs2.ChannelBindingFlag
import rere.sasl.gs2.ChannelBindingFlag.NotSupports
import rere.sasl.scram.client._
import rere.sasl.scram.messages._
import rere.sasl.util.{Base64, Base64String, EscapedString, PrintableString}

import scala.concurrent.Future
import scala.concurrent.duration._

class AuthCommanderTest
  extends TestKit(ActorSystem("AuthCommanderTest", ConfigFactory.parseString(AuthCommanderTest.config)))
  with FlatSpecLike
  with BeforeAndAfterAll
  with MockFactory
  with ScalaFutures
  with Inside
  with Matchers {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
    super.afterAll()
  }

  trait mocks {
    implicit val mat = ActorMaterializer(
      ActorMaterializerSettings.create(system).withInputBuffer(16, 16)
    )

    def base64(str: String): Base64String = {
      Base64.to(java.util.Base64.getDecoder.decode(str))
    }

    val clientFirstStep = stub[ClientFirstStep]
    val clientSecondStep = stub[ClientSecondStep]
    val clientFinalStep = stub[ClientFinalStep]

    val firstMessage = ClientFirstMessage(
      gs2.Header(NotSupports, None),
      ClientFirstMessageBare(
        None,
        EscapedString.to("user"),
        PrintableString("fyko+d2lbbFgONRv9qkxdawL"),
        Nil
      )
    )

    val finalMessage = ClientFinalMessage(
      ClientFinalMessageWithoutProof(
        base64("biws"),
        base64("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
        Nil
      ),
      base64("v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=")
    )

    def getCommander(login: String, password: String): ((TestPublisher.Probe[ByteString], Future[Done]), TestSubscriber.Probe[ByteString]) = {
      val fromServer = TestSource.probe[ByteString]
      val toServer = TestSink.probe[ByteString]
      val logger = Logging(system, "auth-commander-test")
      val authCommander = new AuthCommander(clientFirstStep, login, password, logger)

      fromServer.watchTermination()(Keep.both).via(authCommander).toMat(toServer)(Keep.both).run()
    }
  }

  behavior of "AuthCommander"

  it should "send all messages and finish when all goes well" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage
    clientSecondStep.process _ when * returns Right(clientFinalStep)
    clientFinalStep.finalMessage _ when () returns finalMessage
    clientFinalStep.process _ when * returns Right("OK")

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify ("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"max_protocol_version":0,"min_protocol_version":0,"server_version":"2.3.5~0trusty","success":true}"""))
    fromServer.sendNext(ByteString("""{"authentication":"r=__server_nonce__,s=++salt++,i=1","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(ByteString("""{"authentication":"c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7g==,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts="}"""))

    // server received message, time to validate how client parsed previous message
    clientSecondStep.process _ verify where { firstMessage: ServerFirstMessage =>
      firstMessage.reserved.isEmpty &&
      firstMessage.serverNonce.toString == "__server_nonce__" &&
      firstMessage.salt.toString == "++salt++" &&
      firstMessage.iterationCount == 1 &&
      firstMessage.extensions.isEmpty
    }

    // server responds with 1 message
    fromServer.sendNext(ByteString("""{"authentication":"v=++verifier++","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectComplete()

    // server received complete, time to validate how client parsed previous message
    clientFinalStep.process _ verify where { finalMessage: ServerFinalMessage =>
      finalMessage.errorOrVerifier.right.get.toString == "++verifier++" &&
      finalMessage.extensions == Nil
    }

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when final server message came with bad verifier" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage
    clientSecondStep.process _ when * returns Right(clientFinalStep)
    clientFinalStep.finalMessage _ when () returns finalMessage
    clientFinalStep.process _ when * returns Left(AuthError("Verification failed :("))

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"max_protocol_version":0,"min_protocol_version":0,"server_version":"2.3.5~0trusty","success":true}"""))
    fromServer.sendNext(ByteString("""{"authentication":"r=__server_nonce__,s=++salt++,i=1","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(ByteString("""{"authentication":"c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7g==,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts="}"""))

    // server received message, time to validate how client parsed previous message
    clientSecondStep.process _ verify where { firstMessage: ServerFirstMessage =>
      firstMessage.reserved.isEmpty &&
      firstMessage.serverNonce.toString == "__server_nonce__" &&
      firstMessage.salt.toString == "++salt++" &&
      firstMessage.iterationCount == 1 &&
      firstMessage.extensions.isEmpty
    }

    // server responds with 1 message
    fromServer.sendNext(ByteString("""{"authentication":"v=++bad+verifier++","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlAuthError]
    err.getMessage shouldBe "Auth failed after processing final server message. Verification failed :("

    // server received error, time to validate how client parsed previous message
    clientFinalStep.process _ verify where { finalMessage: ServerFinalMessage =>
      finalMessage.errorOrVerifier.right.get.toString == "++bad+verifier++" &&
      finalMessage.extensions == Nil
    }

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when final server message came with damaged scram message" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage
    clientSecondStep.process _ when * returns Right(clientFinalStep)
    clientFinalStep.finalMessage _ when () returns finalMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"max_protocol_version":0,"min_protocol_version":0,"server_version":"2.3.5~0trusty","success":true}"""))
    fromServer.sendNext(ByteString("""{"authentication":"r=__server_nonce__,s=++salt++,i=1","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(ByteString("""{"authentication":"c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7g==,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts="}"""))

    // server received message, time to validate how client parsed previous message
    clientSecondStep.process _ verify where { firstMessage: ServerFirstMessage =>
      firstMessage.reserved.isEmpty &&
      firstMessage.serverNonce.toString == "__server_nonce__" &&
      firstMessage.salt.toString == "++salt++" &&
      firstMessage.iterationCount == 1 &&
      firstMessage.extensions.isEmpty
    }

    // server responds with 1 message
    fromServer.sendNext(ByteString("""{"authentication":"__damaged_message__","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlProtocolParsingError]
    err.getMessage shouldBe "Can't parse scram message"
    err.getCause shouldBe a [ParseError]

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when final server message can't be parsed" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage
    clientSecondStep.process _ when * returns Right(clientFinalStep)
    clientFinalStep.finalMessage _ when () returns finalMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"max_protocol_version":0,"min_protocol_version":0,"server_version":"2.3.5~0trusty","success":true}"""))
    fromServer.sendNext(ByteString("""{"authentication":"r=__server_nonce__,s=++salt++,i=1","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(ByteString("""{"authentication":"c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7g==,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts="}"""))

    // server received message, time to validate how client parsed previous message
    clientSecondStep.process _ verify where { firstMessage: ServerFirstMessage =>
      firstMessage.reserved.isEmpty &&
      firstMessage.serverNonce.toString == "__server_nonce__" &&
      firstMessage.salt.toString == "++salt++" &&
      firstMessage.iterationCount == 1 &&
      firstMessage.extensions.isEmpty
    }

    // server responds with 1 message
    fromServer.sendNext(ByteString("""{"damaged":true,"success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlProtocolParsingError]
    err.getMessage shouldBe "Can't parse message"
    err.getCause shouldBe a [DecodingFailure]

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when first server message came with bad nonce" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage
    clientSecondStep.process _ when * returns Left(AuthError("Invalid nonce :("))

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"max_protocol_version":0,"min_protocol_version":0,"server_version":"2.3.5~0trusty","success":true}"""))
    fromServer.sendNext(ByteString("""{"authentication":"r=__bad_server_nonce__,s=++salt++,i=1","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlAuthError]
    err.getMessage shouldBe "Auth failed after processing first server message. Invalid nonce :("

    // server sink received error, time to validate how client parsed previous message
    clientSecondStep.process _ verify where { firstMessage: ServerFirstMessage =>
      firstMessage.reserved.isEmpty &&
      firstMessage.serverNonce.toString == "__bad_server_nonce__" &&
      firstMessage.salt.toString == "++salt++" &&
      firstMessage.iterationCount == 1 &&
      firstMessage.extensions.isEmpty
    }

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when first server message came with damaged scram message" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"max_protocol_version":0,"min_protocol_version":0,"server_version":"2.3.5~0trusty","success":true}"""))
    fromServer.sendNext(ByteString("""{"authentication":"__damaged_message__","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlProtocolParsingError]
    err.getMessage shouldBe "Can't parse scram message"
    err.getCause shouldBe a [ParseError]

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when first server message can't be parsed" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"max_protocol_version":0,"min_protocol_version":0,"server_version":"2.3.5~0trusty","success":true}"""))
    fromServer.sendNext(ByteString("""{"damaged":true,"success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlProtocolParsingError]
    err.getMessage shouldBe "Can't parse message"
    err.getCause shouldBe a [DecodingFailure]

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when protocol version is not supported by driver" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"max_protocol_version":1,"min_protocol_version":1,"server_version":"2.3.5~0trusty","success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlDriverError]
    err.getMessage shouldBe "Unsupported protocol version 0, expected between 1 and 1."

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when protocol version message can't be parsed" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"damaged":true,"success":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlProtocolParsingError]
    err.getMessage shouldBe "Can't parse message"
    err.getCause shouldBe a [DecodingFailure]

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when server returned verbose auth error" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"error_code":12,"error":"Verbose auth error","success":false}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlAuthError]
    err.getMessage shouldBe "Verbose auth error"

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when server returned error with some error code" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
        ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"error_code":42,"error":"Bad encoding","success":false}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlDriverError]
    err.getMessage shouldBe "Bad encoding"

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when server returned error without error code" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
        ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"success":false}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlDriverError]
    err.getMessage shouldBe "<no error message, no error code>"

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when server message can't be parsed by general protocol rules" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""{"damaged":true}"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlProtocolParsingError]
    err.getMessage shouldBe """Can't parse message '{"damaged":true}'"""
    err.getCause shouldBe a [DecodingFailure]

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

  it should "raise an error when server message can't be parsed as json" in new mocks {
    clientFirstStep.auth _ when (*, *, *, *, *) returns clientSecondStep
    clientSecondStep.firstMessage _ when () returns firstMessage

    val ((fromServer, doneFuture), toServer) = getCommander("admin", "")

    fromServer.ensureSubscription()
    toServer.ensureSubscription()

    // when server ready to receive data
    toServer.request(1L)
    toServer.expectNext(
      ByteString(0xc3, 0xbd, 0xc2, 0x34) ++ // protocol version
      ByteString("""{"protocol_version":0,"authentication_method":"SCRAM-SHA-256","authentication":"n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"}""")
    )

    clientFirstStep.auth _ verify("admin", "", ChannelBindingFlag.NotSupports, None, Nil)

    // server responds with 2 messages
    fromServer.sendNext(ByteString("""ERROR: some old protocol error"""))

    // when server ready to receive data
    toServer.request(1L)
    val err = toServer.expectError()
    err shouldBe a [ReqlProtocolParsingError]
    err.getMessage shouldBe """Can't parse message 'ERROR: some old protocol error'"""
    err.getCause shouldBe a [ParsingFailure]

    // client can't process previous message, no need to validate it

    fromServer.expectNoMsg(200.millis)    // no cancel from client
    fromServer.sendComplete()             // server will send complete

    // ... and done
    whenReady(doneFuture) { done =>
      done shouldBe a [Done]
    }
  }

}

object AuthCommanderTest {

  val config = """
    akka {
      loglevel = "WARNING"
    }
  """
}