package rere.sasl.scram.parsers

import org.parboiled2._
import org.scalatest.Inspectors._
import org.scalatest.Matchers._
import org.scalatest.{FunSpec, Inside}
import rere.sasl._
import rere.sasl.scram.messages._
import rere.sasl.util.{Base64String, EscapedString, NoCommaString, PrintableString}

import scala.util.{Failure, Success}

class RFC5802ParserTest extends FunSpec with Inside {

  class TestParser(val input: ParserInput) extends Parser with RFC5802Parser

  def parser(str: String) = new TestParser(str)

  describe("ALPHA") {
    it("should parse strings with latin letters") {
      val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

      forEvery(alphabet) { char =>
        assert(parser(char.toString).`ALPHA`.run().isSuccess)
      }
    }

    it("should not parse strings with digits") {
      assert(parser("0").`ALPHA`.run().isFailure)
      assert(parser("9").`ALPHA`.run().isFailure)
    }

    it("should not parse strings with punctuation symbols") {
      assert(parser(" ").`ALPHA`.run().isFailure)
      assert(parser("@").`ALPHA`.run().isFailure)
      assert(parser("[").`ALPHA`.run().isFailure)
      assert(parser("`").`ALPHA`.run().isFailure)
      assert(parser("{").`ALPHA`.run().isFailure)
    }

    it("should not parse strings with cyrillic letters") {
      assert(parser("а").`ALPHA`.run().isFailure)
    }
  }

  describe("DIGIT") {
    it("should parse strings with digits") {
      val alphabet = "0123456789"

      forEvery(alphabet) { char =>
        assert(parser(char.toString).`DIGIT`.run().isSuccess)
      }
    }
  }

  describe("attr-val") {
    it("should parse attribute-value pair") {
      parser("w=123abc").`attr-val`.run() shouldBe Success(AttrVal('w', new NoCommaString("123abc")))
      parser("W=123ABC").`attr-val`.run() shouldBe Success(AttrVal('W', new NoCommaString("123ABC")))
      parser("w=тест").`attr-val`.run() shouldBe Success(AttrVal('w', new NoCommaString("тест")))
    }

    it("should capture value that contains '='") {
      parser("w==123abc=").`attr-val`.run() shouldBe Success(AttrVal('w', new NoCommaString("=123abc=")))
      parser("w==").`attr-val`.run() shouldBe Success(AttrVal('w', new NoCommaString("=")))
    }

    it("should capture value while ',' or NUL is meet") {
      parser("w=123,456").`attr-val`.run() shouldBe Success(AttrVal('w', new NoCommaString("123")))
      parser("w=abc\u0000def").`attr-val`.run() shouldBe Success(AttrVal('w', new NoCommaString("abc")))
    }

    it("should not parse attribute if it not single ALPHA") {
      assert(parser("we=123abc").`attr-val`.run().isFailure)
      assert(parser("0=123abc").`attr-val`.run().isFailure)
      assert(parser("==123abc").`attr-val`.run().isFailure)
      assert(parser("ц=123abc").`attr-val`.run().isFailure)
    }

    it("should not parse empty value or if it starts with stop symbols") {
      assert(parser("w=").`attr-val`.run().isFailure)
      assert(parser("w=,").`attr-val`.run().isFailure)
      assert(parser("w=\u0000").`attr-val`.run().isFailure)
    }
  }

  describe("value") {

    it("should parse non empty string of not (',' | NUL) symbols") {
      parser("abc").`value`.run() shouldBe Success(new NoCommaString("abc"))
      parser("123").`value`.run() shouldBe Success(new NoCommaString("123"))
      parser("тест").`value`.run() shouldBe Success(new NoCommaString("тест"))
      parser("=").`value`.run() shouldBe Success(new NoCommaString("="))
    }

    it("should not parse empty string") {
      assert(parser("").`value`.run().isFailure)
    }

    it("should not parse strings that starts with stop symbol") {
      assert(parser(",").`value`.run().isFailure)
      assert(parser("\u0000").`value`.run().isFailure)
    }
  }

  describe("value-safe-char") {
    it("should parse any unicode symbol except NUL, '=', and ','") {
      val alphabet = "ABCabc012±!@АБВабв\u26f5"

      forEvery(alphabet) { char =>
        assert(parser(char.toString).`value-safe-char`.run().isSuccess)
      }
    }

    it("should not parse empty string") {
      assert(parser("").`value-safe-char`.run().isFailure)
    }

    it("should not parse strings that contains only NUL or '=' or ','") {
      assert(parser("\u0000").`value-safe-char`.run().isFailure)
      assert(parser("=").`value-safe-char`.run().isFailure)
      assert(parser(",").`value-safe-char`.run().isFailure)
    }
  }

  describe("value-char") {
    it("should parse any unicode symbol except NUL and ','") {
      val alphabet = "ABCabc012±!@АБВабв\u26f5="

      forEvery(alphabet) { char =>
        assert(parser(char.toString).`value-char`.run().isSuccess)
      }
    }

    it("should not parse empty string") {
      assert(parser("").`value-char`.run().isFailure)
    }

    it("should not parse strings that contains only NUL or ','") {
      assert(parser("\u0000").`value-char`.run().isFailure)
      assert(parser(",").`value-char`.run().isFailure)
    }
  }

  describe("printable") {
    it("should parse any printable ASCII symbol except ','") {
      val alphabet = """!"#$%&'()*+-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"""

      forEvery(alphabet) { char =>
        assert(parser(char.toString).`printable`.run().isSuccess)
      }
    }

    it("should not parse empty string") {
      assert(parser("").`printable`.run().isFailure)
    }

    it("should not parse string that contains only ','") {
      assert(parser(",").`printable`.run().isFailure)
    }
  }

  describe("base64-char") {
    it("should parse every allowed base64 symbol") {
      val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

      forEvery(alphabet) { char =>
        assert(parser(char.toString).`base64-char`.run().isSuccess)
      }
    }

    it("should not parse empty string") {
      assert(parser("").`base64-char`.run().isFailure)
    }

    it("should not parse not allowed symbols") {
      assert(parser("=").`base64-char`.run().isFailure)
      assert(parser(",").`base64-char`.run().isFailure)
      assert(parser(".").`base64-char`.run().isFailure)
      assert(parser("-").`base64-char`.run().isFailure)
      assert(parser("_").`base64-char`.run().isFailure)
      assert(parser(":").`base64-char`.run().isFailure)
    }
  }

  describe("base64-4") {
    it("should parse full blocks") {
      parser("ABCD").`base64-4`.run() shouldBe Success("ABCD")
      parser("ABCDE").`base64-4`.run() shouldBe Success("ABCD")
    }

    it("should not parse strings that shorted than 4 bytes") {
      assert(parser("ABC").`base64-4`.run().isFailure)
      assert(parser("AB").`base64-4`.run().isFailure)
      assert(parser("A").`base64-4`.run().isFailure)
      assert(parser("").`base64-4`.run().isFailure)
    }

    it("should not parse padded strings") {
      assert(parser("====").`base64-4`.run().isFailure)
      assert(parser("A===").`base64-4`.run().isFailure)
      assert(parser("AB==").`base64-4`.run().isFailure)
      assert(parser("ABC=").`base64-4`.run().isFailure)
    }
  }

  describe("base64-3") {
    it("should parse blocks with 3 symbols and one padding symbol") {
      parser("ABC=").`base64-3`.run() shouldBe Success("ABC=")
      parser("ABC=E").`base64-3`.run() shouldBe Success("ABC=")
    }

    it("should not parse strings that contains less symbols") {
      assert(parser("ABC").`base64-3`.run().isFailure)
      assert(parser("AB").`base64-3`.run().isFailure)
      assert(parser("A").`base64-3`.run().isFailure)
      assert(parser("").`base64-3`.run().isFailure)
    }

    it("should not parse strings with not 1 padding symbol in first 4 symbols") {
      assert(parser("====").`base64-3`.run().isFailure)
      assert(parser("A===").`base64-3`.run().isFailure)
      assert(parser("AB==").`base64-3`.run().isFailure)
      assert(parser("ABCD").`base64-3`.run().isFailure)
    }
  }

  describe("base64-2") {
    it("should parse blocks with 2 symbols and 2 padding symbols") {
      parser("AB==").`base64-2`.run() shouldBe Success("AB==")
      parser("AB==E").`base64-2`.run() shouldBe Success("AB==")
    }

    it("should not parse strings that contains less symbols") {
      assert(parser("ABC").`base64-2`.run().isFailure)
      assert(parser("AB").`base64-2`.run().isFailure)
      assert(parser("A").`base64-2`.run().isFailure)
      assert(parser("").`base64-2`.run().isFailure)
    }

    it("should not parse strings with not 2 padding symbols in first 4 symbols") {
      assert(parser("====").`base64-2`.run().isFailure)
      assert(parser("A===").`base64-2`.run().isFailure)
      assert(parser("ABC=").`base64-2`.run().isFailure)
      assert(parser("ABCD").`base64-2`.run().isFailure)
    }
  }

  describe("base64") {
    it("should parse any valid base64 string") {
      parser("wq9cXyjjg4QpXy/Crw==").`base64`.run() shouldBe
        Success(new Base64String("wq9cXyjjg4QpXy/Crw=="))

      parser("4peVIOKXoSDil5U=").`base64`.run() shouldBe
        Success(new Base64String("4peVIOKXoSDil5U="))

      parser("4LCgX+CwoA==").`base64`.run() shouldBe
        Success(new Base64String("4LCgX+CwoA=="))

      parser("4LKg4pWt4pWu4LKg").`base64`.run() shouldBe
        Success(new Base64String("4LKg4pWt4pWu4LKg"))

      parser("4ZWZKOKHgOKAuOKGvOKAtinhlZc=").`base64`.run() shouldBe
        Success(new Base64String("4ZWZKOKHgOKAuOKGvOKAtinhlZc="))

      parser("KOKVr8Kw4pahwrDvvInila/vuLUg4pS74pSB4pS7").`base64`.run() shouldBe
        Success(new Base64String("KOKVr8Kw4pahwrDvvInila/vuLUg4pS74pSB4pS7"))
    }

    it("should parse by 4 symbols blocks before meet not valid one") {
      parser("ABC!").`base64`.run() shouldBe Success(new Base64String(""))
      parser("ABCD!").`base64`.run() shouldBe Success(new Base64String("ABCD"))
      parser("ABCDE!").`base64`.run() shouldBe Success(new Base64String("ABCD"))
      parser("ABCDEF!").`base64`.run() shouldBe Success(new Base64String("ABCD"))
      parser("ABCDEFG!").`base64`.run() shouldBe Success(new Base64String("ABCD"))
      parser("ABCDEFGH!").`base64`.run() shouldBe Success(new Base64String("ABCDEFGH"))

      parser("====").`base64`.run() shouldBe Success(new Base64String(""))
      parser("====ABC=").`base64`.run() shouldBe Success(new Base64String(""))
      parser("").`base64`.run() shouldBe Success(new Base64String(""))
    }
  }

  describe("posit-number") {
    it("should parse positive numbers") {
      parser("123").`posit-number`.run() shouldBe Success(123)
    }

    it("should not parse zero or negative values") {
      assert(parser("0").`posit-number`.run().isFailure)
      assert(parser("-123").`posit-number`.run().isFailure)
    }

    it("should not parse EXTREMELY DANGEROUS big integers") {
      inside(parser("1234567890123").`posit-number`.run()) {
        case Failure(ex) =>
          ex shouldBe a[NumberFormatException]
      }
    }
  }

  describe("saslname") {
    it("should parse name with ',' and '=' replacements") {
      parser("ABCabc123тест±£¢∞").`saslname`.run() shouldBe Success(new EscapedString("ABCabc123тест±£¢∞"))
      parser("abc=2Cdef=3Dghi").`saslname`.run() shouldBe Success(new EscapedString("abc=2Cdef=3Dghi"))
    }

    it("should not capture ',', '=' and NUL") {
      parser("ABC,abc").`saslname`.run() shouldBe Success(new EscapedString("ABC"))
      parser("ABC=abc").`saslname`.run() shouldBe Success(new EscapedString("ABC"))
      parser("ABC\u0000abc").`saslname`.run() shouldBe Success(new EscapedString("ABC"))
    }

    it("should not allow replacements in lower case") {
      parser("ABC=2c123").`saslname`.run() shouldBe Success(new EscapedString("ABC"))
      parser("ABC=3d123").`saslname`.run() shouldBe Success(new EscapedString("ABC"))
    }

    it("should not parse empty string") {
      assert(parser("").`saslname`.run().isFailure)
    }
  }

  describe("authzid") {
    it("should parse auth id attr-val pair with saslname value") {
      parser("a=ABCabc123тест±£¢∞").`authzid`.run() shouldBe
        Success(new EscapedString("ABCabc123тест±£¢∞"))

      parser("a=abc=2Cdef=3Dghi").`authzid`.run() shouldBe
        Success(new EscapedString("abc=2Cdef=3Dghi"))
    }

    it("should not parse upper case alternative") {
      assert(parser("A=ABC").`authzid`.run().isFailure)
    }

    it("should not parse pair with empty value") {
      assert(parser("a=").`authzid`.run().isFailure)
    }
  }

  describe("cb-name") {
    it("should parse channel binding name") {
      parser("tls-1.2-custom").`cb-name`.run() shouldBe Success("tls-1.2-custom")
      parser("tls-server-end-point").`cb-name`.run() shouldBe Success("tls-server-end-point")
      parser("tls-unique").`cb-name`.run() shouldBe Success("tls-unique")
    }

    it("should not capture not allowed symbols") {
      assert(parser(" ").`cb-name`.run().isFailure)
      assert(parser("_").`cb-name`.run().isFailure)
      assert(parser("/").`cb-name`.run().isFailure)
    }

    it("should not parse empty string") {
      assert(parser("").`cb-name`.run().isFailure)
    }
  }

  describe("gs2-cbind-flag") {
    import gs2.ChannelBindingFlag._

    it("should parse SupportsAndUsed with channel binding name if client requires channel binding") {
      parser("p=tls-unique").`gs2-cbind-flag`.run() shouldBe Success(SupportsAndUsed("tls-unique"))
    }

    it("should parse NotSupports if client doesn't support channel binding") {
      parser("n").`gs2-cbind-flag`.run() shouldBe Success(NotSupports)
    }

    it("should parse SupportsButNotUsed if client does support channel binding but thinks the server does not") {
      parser("y").`gs2-cbind-flag`.run() shouldBe Success(SupportsButNotUsed)
    }

    it("should not parse empty string") {
      assert(parser("").`gs2-cbind-flag`.run().isFailure)
    }

    it("should not parse upper case alernatives") {
      assert(parser("P=tls-unique").`gs2-cbind-flag`.run().isFailure)
      assert(parser("N").`gs2-cbind-flag`.run().isFailure)
      assert(parser("Y").`gs2-cbind-flag`.run().isFailure)
    }
  }

  describe("gs2-header") {
    import gs2.ChannelBindingFlag._

    it("should parse channel binding flag and optional auth id") {
      parser("p=tls-unique-1.2,a=user=3Dtest,").`gs2-header`.run() shouldBe
        Success(gs2.Header(SupportsAndUsed("tls-unique-1.2"), Some(new EscapedString("user=3Dtest"))))

      parser("n,,").`gs2-header`.run() shouldBe
        Success(gs2.Header(NotSupports, None))

      parser("y,,").`gs2-header`.run() shouldBe
        Success(gs2.Header(SupportsButNotUsed, None))
    }

    it("should not parse header with omitted commas") {
      assert(parser("n,").`gs2-header`.run().isFailure)
      assert(parser("n").`gs2-header`.run().isFailure)
    }

    it("should not parse empty string") {
      assert(parser("").`gs2-header`.run().isFailure)
    }

    it("should not capture next attr-val pairs") {
      parser("n,,n=user").`gs2-header`.run() shouldBe Success(gs2.Header(NotSupports, None))
    }
  }

  describe("username") {
    it("should parse username") {
      parser("n=user").`username`.run() shouldBe Success(new EscapedString("user"))
      parser("n=пользователь").`username`.run() shouldBe Success(new EscapedString("пользователь"))
    }

    it("should not parse upper case alternative") {
      assert(parser("N=user").`username`.run().isFailure)
    }

    it("should not parse empty string") {
      assert(parser("").`username`.run().isFailure)
    }
  }

  describe("reserved-mext") {
    it("should parse attr-val pair") {
      parser("m=ABC").`reserved-mext`.run() shouldBe Success(AttrVal('m', new NoCommaString("ABC")))
    }

    it("should not parse pair with empty value") {
      assert(parser("m=").`reserved-mext`.run().isFailure)
    }

    it("should not parse empty setring") {
      assert(parser("").`reserved-mext`.run().isFailure)
    }

    it("should not parse upper case alternative") {
      assert(parser("M=").`reserved-mext`.run().isFailure)
    }
  }

  describe("channel-binding") {
    it("should parse base64 encoded GS2 header") {
      parser("c=biws").`channel-binding`.run() shouldBe
        Success(new Base64String("biws"))

      parser("c=cD10bHMtdW5pcXVlLTEuMixhPXVzZXI9M0R0ZXN0LA==").`channel-binding`.run() shouldBe
        Success(new Base64String("cD10bHMtdW5pcXVlLTEuMixhPXVzZXI9M0R0ZXN0LA=="))

      parser("c=bixhPWFiYyw=").`channel-binding`.run() shouldBe
        Success(new Base64String("bixhPWFiYyw="))
    }

    it("should parse pair with empty value") {
      parser("c=").`channel-binding`.run() shouldBe Success(new Base64String(""))
    }

    it("should not capture value that starts with not base64 symbols") {
      parser("c=,").`channel-binding`.run() shouldBe Success(new Base64String(""))
    }

    it("should not parse upper case alternative") {
      assert(parser("C=").`channel-binding`.run().isFailure)
    }
  }

  describe("proof") {
    it("should parse base64 encoded proof") {
      parser("p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=").`proof`.run() shouldBe
        Success(new Base64String("v0X8v3Bz2T0CJGbJQyF0X+HI4Ts="))
    }

    it("should parse pair with empty value") {
      parser("p=").`proof`.run() shouldBe Success(new Base64String(""))
    }

    it("should not parse upper case alternative") {
      assert(parser("P=").`proof`.run().isFailure)
    }
  }

  describe("nonce") {
    it("should parse client and server nonce") {
      parser("r=fyko+d2lbbFgONRv9qkxdawL").`nonce`.run() shouldBe
        Success(new PrintableString("fyko+d2lbbFgONRv9qkxdawL"))

      parser("r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j").`nonce`.run() shouldBe
        Success(new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"))
    }

    it("should capture printable symbols until ',' is meet") {
      parser("r=abc,def").`nonce`.run() shouldBe Success(new PrintableString("abc"))
    }

    it("should not parse empty string") {
      assert(parser("r=").`nonce`.run().isFailure)
    }

    it("should not parse upper case alternative") {
      assert(parser("R=").`nonce`.run().isFailure)
    }
  }

  describe("salt") {
    it("should parse pair with salt") {
      parser("s=QSXCR+Q6sek8bf92").`salt`.run() shouldBe
        Success(new Base64String("QSXCR+Q6sek8bf92"))
    }

    it("should parse pair with empty value") {
      parser("s=").`salt`.run() shouldBe Success(new Base64String(""))
    }

    it("should not parse upper case alternative") {
      assert(parser("S=").`salt`.run().isFailure)
    }
  }

  describe("verifier") {
    it("should parse base64 encoded ServerSignature") {
      parser("v=rmF9pqV8S7suAoZWja4dJRkFsKQ=").`verifier`.run() shouldBe
        Success(new Base64String("rmF9pqV8S7suAoZWja4dJRkFsKQ="))
    }

    it("should parse pair with empty value") {
      parser("v=").`verifier`.run() shouldBe Success(new Base64String(""))
    }

    it("should not parse upper case alternative") {
      assert(parser("V=").`verifier`.run().isFailure)
    }
  }

  describe("iteration-count") {
    it("should parse iteration count pair") {
      parser("i=4096").`iteration-count`.run() shouldBe Success(4096)
    }

    it("should not parse zero and negative values") {
      assert(parser("i=0").`iteration-count`.run().isFailure)
      assert(parser("i=-16").`iteration-count`.run().isFailure)
    }

    it("should not parse very big numbers") {
      inside(parser("i=4294967296").`iteration-count`.run()) {
        case Failure(ex) =>
          ex shouldBe a[NumberFormatException]
      }
    }

    it("should not parse empty value") {
      assert(parser("i=").`iteration-count`.run().isFailure)
      assert(parser("").`iteration-count`.run().isFailure)
    }

    it("should not parse upper case alternative") {
      assert(parser("I=4096").`iteration-count`.run().isFailure)
    }
  }

  describe("client-first-message-bare") {
    it("should parse client first message bare") {
      parser("n=user,r=fyko+d2lbbFgONRv9qkxdawL").`client-first-message-bare`.run() shouldBe
        Success(ClientFirstMessageBare(
          None, new EscapedString("user"), new PrintableString("fyko+d2lbbFgONRv9qkxdawL"), Seq.empty
        ))

      parser("m=abc==def,n=∑√µ=3D∞,r=dGVzdA==,j=ext1.1=ext1.2,k=ext2.1=ext2.2,l==?").
        `client-first-message-bare`.run() shouldBe
        Success(ClientFirstMessageBare(
          Some(AttrVal('m', new NoCommaString("abc==def"))),
          new EscapedString("∑√µ=3D∞"),
          new PrintableString("dGVzdA=="),
          Seq(
            AttrVal('j', new NoCommaString("ext1.1=ext1.2")),
            AttrVal('k', new NoCommaString("ext2.1=ext2.2")),
            AttrVal('l', new NoCommaString("=?"))
          )
        ))
    }
  }

  describe("client-first-message") {
    import gs2.ChannelBindingFlag._

    it("should parse full client first message") {
      parser("n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL").
        `client-first-message`.run() shouldBe
        Success(ClientFirstMessage(
          gs2.Header(NotSupports, None),
          ClientFirstMessageBare(
            None, new EscapedString("user"), new PrintableString("fyko+d2lbbFgONRv9qkxdawL"), Seq.empty
          )
        ))
    }

    it("should parse first message with escaped username") {
      parser("n,,n=it=3Dis=2Cusername,r=fyko+d2lbbFgONRv9qkxdawL").
        `client-first-message`.run() shouldBe
        Success(ClientFirstMessage(
          gs2.Header(NotSupports, None),
          ClientFirstMessageBare(
            None, new EscapedString("it=3Dis=2Cusername"), new PrintableString("fyko+d2lbbFgONRv9qkxdawL"), Seq.empty
          )
        ))
    }
  }

  describe("server-first-message") {
    it("should parse server first message") {
      parser("r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096").
        `server-first-message`.run() shouldBe
        Success(ServerFirstMessage(
          None,
          new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
          new Base64String("QSXCR+Q6sek8bf92"),
          4096,
          Seq.empty
        ))
    }
  }

  describe("client-final-message-without-proof") {
    it("should parse client final message without proof pair") {
      parser("c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j").
        `client-final-message-without-proof`.run() shouldBe
        Success(ClientFinalMessageWithoutProof(
          new Base64String("biws"),
          new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
          Seq.empty
        ))
    }

    it("should parse extensions") {
      parser("c=biws,r=fyko,j=ext1.1=ext1.2,k=ext2.1=ext2.2").
        `client-final-message-without-proof`.run() shouldBe
        Success(ClientFinalMessageWithoutProof(
          new Base64String("biws"),
          new PrintableString("fyko"),
          Seq(
            AttrVal('j', new NoCommaString("ext1.1=ext1.2")),
            AttrVal('k', new NoCommaString("ext2.1=ext2.2"))
          )
        ))
    }
  }

  describe("client-final-message") {
    it("should parse client final message with proof pair") {
      parser("c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=").
      `client-final-message`.run() shouldBe
      Success(ClientFinalMessage(
        ClientFinalMessageWithoutProof(
          new Base64String("biws"),
          new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
          Seq.empty
        ),
        new Base64String("v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=")
      ))
    }

    it("should not capture proof as part of extensions pairs") {
      parser("c=,r=#,p=abcd").`client-final-message`.run() shouldBe
        Success(ClientFinalMessage(
          ClientFinalMessageWithoutProof(
            new Base64String(""),
            new PrintableString("#"),
            Seq.empty
          ),
          new Base64String("abcd")
        ))

      parser("c=,r=#,w=$,k=%,p=abcd").`client-final-message`.run() shouldBe
        Success(ClientFinalMessage(
          ClientFinalMessageWithoutProof(
            new Base64String(""),
            new PrintableString("#"),
            Seq(
              AttrVal('w', new NoCommaString("$")),
              AttrVal('k', new NoCommaString("%"))
            )
          ),
          new Base64String("abcd")
        ))
    }
  }

  describe("server-error") {
    it("should parse server error message") {
      parser("e=invalid-encoding").`server-error`.run() shouldBe
        Success(ServerError(ServerErrorType.InvalidEncoding))

      parser("e=something-goes-wrong").`server-error`.run() shouldBe
        Success(ServerError(
          ServerErrorType.UnrecognizedError(
            new NoCommaString("something-goes-wrong")
          )
        ))
    }

    it("should not parse pair with empty value") {
      assert(parser("e=").`server-error`.run().isFailure)
    }

    it("should not parse upper case alternative") {
      assert(parser("E=").`server-error`.run().isFailure)
    }
  }

  describe("server-error-value") {
    import ServerErrorType._

    it("should parse predefined error messages") {
      parser("invalid-encoding").`server-error-value`.run() shouldBe Success(InvalidEncoding)
      parser("extensions-not-supported").`server-error-value`.run() shouldBe Success(ExtensionsNotSupported)
      parser("invalid-proof").`server-error-value`.run() shouldBe Success(InvalidProof)
      parser("channel-bindings-dont-match").`server-error-value`.run() shouldBe Success(ChannelBindingsDontMatch)
      parser("server-does-support-channel-binding").`server-error-value`.run() shouldBe Success(ServerDoesSupportChannelBinding)
      parser("channel-binding-not-supported").`server-error-value`.run() shouldBe Success(ChannelBindingNotSupported)
      parser("unsupported-channel-binding-type").`server-error-value`.run() shouldBe Success(UnsupportedChannelBindingType)
      parser("unknown-user").`server-error-value`.run() shouldBe Success(UnknownUser)
      parser("invalid-username-encoding").`server-error-value`.run() shouldBe Success(InvalidUsernameEncoding)
      parser("no-resources").`server-error-value`.run() shouldBe Success(NoResources)
      parser("other-error").`server-error-value`.run() shouldBe Success(OtherError)
      parser("crash").`server-error-value`.run() shouldBe Success(UnrecognizedError(new NoCommaString("crash")))
    }

    it("should not parse predefined error messages in upper case") {
      parser("INVALID-ENCODING").`server-error-value`.run() shouldBe
        Success(UnrecognizedError(new NoCommaString("INVALID-ENCODING")))
    }

    it("should not parse empty string") {
      assert(parser("").`server-error-value`.run().isFailure)
    }
  }

  describe("server-error-value-ext") {
    it("should parse unknown server errors") {
      parser("error a=0").`server-error-value-ext`.run() shouldBe Success(new NoCommaString("error a=0"))
    }

    it("should not capture stop symbols") {
      parser("error, server crashed").`server-error-value-ext`.run() shouldBe
        Success(new NoCommaString("error"))

      parser("error\u0000memory corrupted").`server-error-value-ext`.run() shouldBe
        Success(new NoCommaString("error"))
    }

    it("should not parse empty string") {
      assert(parser("").`server-error-value-ext`.run().isFailure)
    }
  }

  describe("server-final-message") {
    it("should parse server final message with verifier") {
      parser("v=rmF9pqV8S7suAoZWja4dJRkFsKQ=").`server-final-message`.run() shouldBe
        Success(ServerFinalMessage(
          Right(new Base64String("rmF9pqV8S7suAoZWja4dJRkFsKQ=")),
          Seq.empty
        ))
    }

    it("should parse verifier with extensions") {
      parser("v=,j=ext1.1=ext1.2,k=ext2.1=ext2.2").`server-final-message`.run() shouldBe
        Success(ServerFinalMessage(
          Right(new Base64String("")),
          Seq(
            AttrVal('j', new NoCommaString("ext1.1=ext1.2")),
            AttrVal('k', new NoCommaString("ext2.1=ext2.2"))
          )
        ))
    }

    it("should parse server error with extensions") {
      parser("e=invalid-proof,j=ext1.1=ext1.2,k=ext2.1=ext2.2").`server-final-message`.run() shouldBe
        Success(ServerFinalMessage(
          Left(ServerError(ServerErrorType.InvalidProof)),
          Seq(
            AttrVal('j', new NoCommaString("ext1.1=ext1.2")),
            AttrVal('k', new NoCommaString("ext2.1=ext2.2"))
          )
        ))
    }

    it("should not parse empty string") {
      assert(parser("").`server-final-message`.run().isFailure)
    }
  }

  describe("extensions") {
    it("should parse one attribute-value pair") {
      parser("j=ext1.1=ext1.2").`extensions`.run() shouldBe
        Success(Seq(AttrVal('j', new NoCommaString("ext1.1=ext1.2"))))
    }

    it("should parse many pairs keeping order") {
      parser("j=ext=1,k=ext=2,l=ext=3").`extensions`.run() shouldBe
        Success(Seq(
          AttrVal('j', new NoCommaString("ext=1")),
          AttrVal('k', new NoCommaString("ext=2")),
          AttrVal('l', new NoCommaString("ext=3"))
        ))
    }

    it("should not parse pair with not exactly one ALPHA symbol attribute") {
      assert(parser("=123").`extensions`.run().isFailure)
      assert(parser("ab=123").`extensions`.run().isFailure)
      assert(parser("1=1").`extensions`.run().isFailure)
    }

    it("should not capture 'p' attribute that can go after extensions") {
      parser("j=123,p=abc,l=456").`extensions`.run() shouldBe
        Success(Seq(
          AttrVal('j', new NoCommaString("123"))
        ))

      parser("j=123,k=456,p=abc").`extensions`.run() shouldBe
        Success(Seq(
          AttrVal('j', new NoCommaString("123")),
          AttrVal('k', new NoCommaString("456"))
        ))
    }

    it("should not parse if 'p' attribute is first") {
      assert(parser("p=abc").`extensions`.run().isFailure)
    }

    it("should not capture ',' if no pair after") {
      parser("j=123,k=456,").`extensions`.run() shouldBe
        Success(Seq(
          AttrVal('j', new NoCommaString("123")),
          AttrVal('k', new NoCommaString("456"))
        ))
    }
  }

}
