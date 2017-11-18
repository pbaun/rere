package rere.sasl.scram.parsers

import org.parboiled2._
import rere.sasl._
import rere.sasl.gs2.ChannelBindingFlag._
import rere.sasl.scram.messages.ServerErrorType._
import rere.sasl.scram.messages._
import rere.sasl.util._

/**
  * 1. Maybe issue in rfc5802:
  *   Section 5.1 of rfc5802 says:
  *     All attribute names are single US-ASCII letters and are case-sensitive.
  *   In section 7 of rfc5802 attr-val defined as
  *     authzid         = "a=" saslname
  *   But sesction 2.3 of rfc5234 says:
  *     ABNF strings are case insensitive and the character set for these strings is US-ASCII
  *   This means that A=saslname also valid form of authzid
  *   Case-sensitive definition of authzid should look like that:
  *     authzid         = %d97 %d61 saslname
  *
  * 2. Maybe issue in rfc5802:
  *   nonce defined as:
  *     nonce           = "r=" c-nonce [s-nonce]
  *     c-nonce         = printable
  *     s-nonce         = printable
  *     printable       = %x21-2B / %x2D-7E
  *
  *   This means that nonce defined like:
  *     nonce           = "r=" Char [Char]
  *
  *   Maybe c-nonce and s-nonce should be defined like:
  *     c-nonce         = 1*printable
  *     s-nonce         = 1*printable
  *
  * */
trait RFC5802Parser {
  this: Parser =>

  // rfc5234#appendix-B.1
  def `ALPHA`: Rule0 = rule {
    CharPredicate('\u0041' to '\u005a') ++ CharPredicate('\u0061' to '\u007a')
  }

  // rfc5234#appendix-B.1
  def `DIGIT`: Rule0 = rule { CharPredicate('\u0030' to '\u0039') }

  // In rfc5802 it used for validation of base64 encoded part of channel-binding
  // rfc5234#appendix-B.1
  //def `OCTET` = rule { CharPredicate.All }

  //UTF-8 rules not used because this parser operates UTF-16 chars

  //Section 5.1: All attribute names are single US-ASCII letters and are case-sensitive
  def `attr-val`: Rule1[AttrVal] = rule {
    capture(`ALPHA`) ~ '=' ~!~ `value` ~>
    ((attribute: String, value: NoCommaString) => AttrVal(attribute.charAt(0), value))
  }

  def `value`: Rule1[NoCommaString] = rule {
    capture(oneOrMore(`value-char`)) ~> ((value: String) => new NoCommaString(value))
  }

  def `value-safe-char`: Rule0 = rule { noneOf("\u0000\u002c\u003d") }

  def `value-char`: Rule0 = rule { `value-safe-char` | '=' }

  def `printable`: Rule0 = rule {
    CharPredicate('\u0021' to '\u002b') ++ CharPredicate('\u002d' to '\u007e')
  }

  def `base64-char`: Rule0 = rule { `ALPHA` | `DIGIT` | '/' | '+' }
  def `base64-4`: Rule1[String] = rule { capture(4.times(`base64-char`)) }
  def `base64-3`: Rule1[String] = rule { capture(3.times(`base64-char`) ~ '=') }
  def `base64-2`: Rule1[String] = rule { capture(2.times(`base64-char`) ~ "==") }
  def `base64`: Rule1[Base64String] = rule {
    zeroOrMore(`base64-4`) ~ optional(`base64-3` | `base64-2`) ~>
    ((fullBlocks: Seq[String], partialBlock: Option[String]) =>
      new Base64String((fullBlocks ++ partialBlock).mkString))
  }

  def `posit-number`: Rule1[Int] = rule {
    capture(CharPredicate('\u0031' to '\u0039') ~ zeroOrMore(`DIGIT`)) ~> ((num: String) => num.toInt)
  }

  def `saslname`: Rule1[EscapedString] = rule {
    capture(oneOrMore(`value-safe-char` | "=2C" | "=2c" | "=3D" | "=3d")) ~>
    ((name: String) => new EscapedString(name))
  }

  def `authzid`: Rule1[EscapedString] = rule {
    "a=" ~ `saslname`
  }

  def `cb-name`: Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.Alpha ++ CharPredicate.Digit ++ '.' ++ '-'))
  }

  def `gs2-cbind-flag`: Rule1[gs2.ChannelBindingFlag] = rule {
    "p=" ~ `cb-name` ~> ((name: String) => SupportsAndUsed(name)) |
    'n' ~ push(NotSupports) |
    'y' ~ push(SupportsButNotUsed)
  }

  def `gs2-header`: Rule1[gs2.Header] = rule {
    `gs2-cbind-flag` ~ ',' ~ optional(`authzid`) ~ ',' ~> (gs2.Header(_, _))
  }

  def `username`: Rule1[EscapedString] = rule {
    "n=" ~ saslname
  }

  def `reserved-mext`: Rule1[AttrVal] = rule {
    "m=" ~ push('m') ~ capture(oneOrMore(`value-char`)) ~>
    ((attribute: Char, value: String) => AttrVal(attribute, new NoCommaString(value)))
  }

  def `channel-binding`: Rule1[Base64String] = rule { "c=" ~ `base64` }

  def `proof`: Rule1[Base64String] = rule { "p=" ~ `base64` }

  def `nonce`: Rule1[PrintableString] = rule {
    "r=" ~ capture(oneOrMore(`printable`)) ~>
    ((nonce: String) => new PrintableString(nonce))
  }

  def `salt`: Rule1[Base64String] = rule { "s=" ~ `base64` }

  def `verifier`: Rule1[Base64String] = rule { "v=" ~ `base64` }

  def `iteration-count`: Rule1[Int] = rule { "i=" ~ `posit-number` }

  def `client-first-message-bare`: Rule1[ClientFirstMessageBare] = rule {
    optional(`reserved-mext` ~ ',') ~
    `username` ~ ',' ~ `nonce` ~
    optional(',' ~ `extensions`) ~>
    ((reserved: Option[AttrVal], username: EscapedString, nonce: PrintableAndSafe, ext: Option[Seq[AttrVal]]) =>
      ClientFirstMessageBare(
        reserved = reserved,
        username = username,
        clientNonce = nonce,
        extensions = ext.getOrElse(Seq.empty[AttrVal])
      )
    )
  }

  def `client-first-message`: Rule1[ClientFirstMessage] = rule {
    `gs2-header` ~ `client-first-message-bare` ~> ClientFirstMessage
  }

  def `server-first-message`: Rule1[ServerFirstMessage] = rule {
    optional(`reserved-mext` ~ ',') ~
    `nonce` ~ ',' ~ `salt` ~ ',' ~ `iteration-count` ~
    optional(',' ~ `extensions`) ~>
    ((reserved: Option[AttrVal], nonce: PrintableAndSafe, salt: Base64String,
      i: Int, ext: Option[Seq[AttrVal]]) =>
      ServerFirstMessage(
        reserved = reserved,
        serverNonce = nonce,
        salt = salt,
        iterationCount = i,
        extensions = ext.getOrElse(Seq.empty[AttrVal])
      )
    )
  }

  def `client-final-message-without-proof`: Rule1[ClientFinalMessageWithoutProof] = rule {
    `channel-binding` ~ ',' ~ `nonce` ~
    optional(',' ~ `extensions`) ~>
    ((channelBinding: Base64String, nonce: PrintableAndSafe, ext: Option[Seq[AttrVal]]) =>
      ClientFinalMessageWithoutProof(
        channelBinding = channelBinding,
        nonce = nonce,
        extensions = ext.getOrElse(Seq.empty[AttrVal])
      )
    )
  }

  def `client-final-message`: Rule1[ClientFinalMessage] = rule {
    `client-final-message-without-proof` ~ ',' ~ `proof` ~> ClientFinalMessage
  }

  def `server-error`: Rule1[ServerError] = rule {
    "e=" ~ `server-error-value` ~> ServerError
  }

  def `server-error-value`: Rule1[ServerErrorType] = rule {
    "invalid-encoding" ~ push(InvalidEncoding) |
    "extensions-not-supported" ~ push(ExtensionsNotSupported) |
    "invalid-proof" ~ push(InvalidProof) |
    "channel-bindings-dont-match" ~ push(ChannelBindingsDontMatch) |
    "server-does-support-channel-binding" ~ push(ServerDoesSupportChannelBinding) |
    "channel-binding-not-supported" ~ push(ChannelBindingNotSupported) |
    "unsupported-channel-binding-type" ~ push(UnsupportedChannelBindingType) |
    "unknown-user" ~ push(UnknownUser) |
    "invalid-username-encoding" ~ push(InvalidUsernameEncoding) |
    "no-resources" ~ push(NoResources) |
    "other-error" ~ push(OtherError) |
    (`server-error-value-ext` ~> ((msg: SafeString) => UnrecognizedError(msg)))
  }

  def `server-error-value-ext`: Rule1[SafeString] = rule { `value` }

  def `server-final-message`: Rule1[ServerFinalMessage] = rule {
    (
      `server-error` ~> ((error: ServerError) => Left(error)) |
      `verifier` ~> ((verifier: Base64String) => Right(verifier))
    ) ~
    optional(',' ~ `extensions`) ~>
    ((errorOrVerifier: Either[ServerError, Base64String], ext: Option[Seq[AttrVal]]) =>
      ServerFinalMessage(
        errorOrVerifier = errorOrVerifier,
        extensions = ext.getOrElse(Seq.empty[AttrVal])
      )
    )
  }

  def `extensions`: Rule1[Seq[AttrVal]] = rule {
    (&(!"p=") ~ `attr-val` ~> ((attrVal: AttrVal) => Seq(attrVal))) ~
    zeroOrMore(',' ~ &(!"p=") ~ `attr-val` ~> ((tail: Seq[AttrVal], head: AttrVal) => head +: tail)) ~>
    ((attrVals: Seq[AttrVal]) => attrVals.reverse)
  }

  // In rfc5802 it used for validation of base64 encoded part of channel-binding
  //def `cbind-data` = rule { oneOrMore(`OCTET`) }

  // In rfc5802 it used for validation of base64 encoded part of channel-binding
  //def `cbind-input` = rule { `gs2-header` ~ optional(`cbind-data`) }
}
