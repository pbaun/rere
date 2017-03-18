package rere.sasl.scram.parsers

import org.parboiled2._
import rere.sasl._
import rere.sasl.gs2.ChannelBindingFlag._
import rere.sasl.scram.messages.ServerErrorType._
import rere.sasl.scram.messages._
import rere.sasl.util._

trait RFC5802Parser {
  this: Parser =>

  //TODO: attr-val is case sensitive (rfc5802#section-5.1) but in protocol used "a=" (e.g. authzid = "a=" saslname).
  //      rfc5234 says that "abc" form is case insensitive.
  //        ABNF strings are case insensitive and the character set for these
  //        strings is US-ASCII.
  //      It means that A= is valid form too.

  // rfc5234#appendix-B.1
  def `ALPHA`: Rule0 = rule {
    CharPredicate('\u0041' to '\u005a') ++ CharPredicate('\u0061' to '\u007a')
  }

  // rfc5234#appendix-B.1
  def `DIGIT`: Rule0 = rule { CharPredicate('\u0030' to '\u0039') }

  //TODO: not imported in RFC5802
  // rfc5234#appendix-B.1
  //def `OCTET` = rule { CharPredicate.All }

  //UTF-8 rules not used because this parser operates UTF-16 chars

  //All attribute names are single US-ASCII letters and are case-sensitive
  def `attr-val`: Rule1[AttrVal] = rule {
    capture(`ALPHA`) ~ '=' ~ `value` ~>
    ((attribute: String, value: NoCommaString) => AttrVal(attribute.charAt(0), value))
  }

  def `value`: Rule1[NoCommaString] = rule {
    capture(oneOrMore(`value-char`)) ~> ((value: String) => new NoCommaString(value))
  }

  def `value-safe-char`: Rule0 = rule { noneOf("\u0000\u002c\u003d") } //TODO: \ud800 - \udfff not allowed two

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

  //case-sensitive =2C and =3D
  def `saslname`: Rule1[EscapedString] = rule {
    capture(oneOrMore(`value-safe-char` | "=2C" | "=3D")) ~>
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

  //TODO: seems like error in rfc: "r=" Char Char
  /*def `nonce`: Rule1[String] = rule {
    "r=" ~ capture(`c-nonce`) ~ optional(capture(`s-nonce`)) ~>
    ((clientNonce: String, serverNonce: Option[String]) => {
      serverNonce match {
        case Some(sn) => clientNonce + sn
        case None => clientNonce
      }
    })
  }
  def `c-nonce` = rule { `printable` }
  def `s-nonce` = rule { `printable` }*/

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

  //proof adds after extensions and we must check that it will not be captured as extensions
  def `extensions`: Rule1[Seq[AttrVal]] = rule {
    (&(!"p=") ~ `attr-val` ~> ((attrVal: AttrVal) => Seq(attrVal))) ~
    zeroOrMore(',' ~ &(!"p=") ~ `attr-val` ~> ((tail: Seq[AttrVal], head: AttrVal) => head +: tail)) ~>
    ((attrVals: Seq[AttrVal]) => attrVals.reverse)
  }

  //def `cbind-data` = rule { oneOrMore(`OCTET`) }

  //def `cbind-input` = rule { `gs2-header` ~ optional(`cbind-data`) }
}
