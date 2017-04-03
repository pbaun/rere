package rere.sasl.scram.rendering

import org.scalatest.Matchers._
import org.scalatest.WordSpec
import rere.sasl.gs2
import rere.sasl.gs2.ChannelBindingFlag
import rere.sasl.scram.messages._
import rere.sasl.scram.rendering.SCRAMRenderer.renderToString
import rere.sasl.util._

class SCRAMRenderingTest extends WordSpec {

  "escapedStringRendering" should {
    "render EscapedString as is" in {
      renderToString(new EscapedString("=2C=3D")) shouldBe "=2C=3D"
    }
  }

  "noCommaStringRendering" should {
    "render NoCommaString as is" in {
      renderToString(new NoCommaString("abc+=")) shouldBe "abc+="
    }
  }

  "printableStringRendering" should {
    "render PrintableString as is" in {
      renderToString(new PrintableString("abc+=,")) shouldBe "abc+=,"
    }
  }

  "base64StringRendering" should {
    "render Base64String as is" in {
      renderToString(new Base64String("QSXCR+Q6sek8bf92")) shouldBe "QSXCR+Q6sek8bf92"
    }
  }

  "printableAndSafeRendering" should {
    "render PrintableString as is" in {
      renderToString(new PrintableString("abc+=,"): PrintableAndSafe) shouldBe "abc+=,"
    }

    "render Base64String as is" in {
      renderToString(new Base64String("QSXCR+Q6sek8bf92"): PrintableAndSafe) shouldBe "QSXCR+Q6sek8bf92"
    }
  }

  "safeStringRendering" should {
    "render EscapedString as is" in {
      renderToString(new EscapedString("=2C=3D"): SafeString) shouldBe "=2C=3D"
    }

    "render NoCommaString as is" in {
      renderToString(new NoCommaString("abc+="): SafeString) shouldBe "abc+="
    }

    "render PrintableAndSafe as is" in {
      renderToString(new PrintableString("abc+=,"): SafeString) shouldBe "abc+=,"
    }
  }

  "channelBindingFlagRendering" should {
    import ChannelBindingFlag._

    "render ChannelBindingFlag.SupportsAndUsed" in {
      val flag: ChannelBindingFlag = SupportsAndUsed("tls-unique")

      renderToString(flag) shouldBe "p=tls-unique"
    }

    "render ChannelBindingFlag.NotSupports" in {
      val flag: ChannelBindingFlag = NotSupports

      renderToString(flag) shouldBe "n"
    }

    "render ChannelBindingFlag.SupportsButNotUsed" in {
      val flag: ChannelBindingFlag = SupportsButNotUsed

      renderToString(flag) shouldBe "y"
    }
  }

  "headerRendering" should {
    import ChannelBindingFlag._
    import gs2.Header

    "render header with authId" in {
      val header: Header = Header(SupportsAndUsed("tls-unique"), Some(EscapedString.to("admin")))

      renderToString(header) shouldBe "p=tls-unique,a=admin,"
    }

    "render header without authId" in {
      val header: Header = Header(NotSupports, None)

      renderToString(header) shouldBe "n,,"
    }
  }

  "attrValRendering" should {
    "render attr val" in {
      val attrVal = AttrVal('k', EscapedString.to("value"))

      renderToString(attrVal) shouldBe "k=value"
    }
  }

  "extensionsRendering" should {
    "render Seq[AttrVal]" in {
      val attrVals = Seq(
        AttrVal('k', EscapedString.to("value")),
        AttrVal('l', EscapedString.to("value2"))
      )

      renderToString(attrVals) shouldBe "k=value,l=value2"
    }
  }

  "clientFirstMessageBareRendering" should {
    "render ClientFirstMessageBare" in {
      val msg = ClientFirstMessageBare(
        Some(AttrVal('x', EscapedString.to("reserved"))),
        EscapedString.to("user"),
        new PrintableString("fyko+d2lbbFgONRv9qkxdawL"),
        Seq(
          AttrVal('y', EscapedString.to("yValue")),
          AttrVal('z', EscapedString.to("zValue"))
        )
      )

      renderToString(msg) shouldBe "x=reserved,n=user,r=fyko+d2lbbFgONRv9qkxdawL,y=yValue,z=zValue"
    }
  }

  "clientFirstMessageRendering" should {
    "render ClientFirstMessage" in {
      val msg = ClientFirstMessage(
        gs2.Header(ChannelBindingFlag.NotSupports, None),
        ClientFirstMessageBare(
          None,
          EscapedString.to("user"),
          new PrintableString("rOprNGfwEbeRWgbNEkqO"),
          Nil
        )
      )

      renderToString(msg) shouldBe "n,,n=user,r=rOprNGfwEbeRWgbNEkqO"
    }
  }

  "serverFirstMessageRendering" should {
    "render ServerFirstMessage" in {
      val msg = ServerFirstMessage(
        Some(AttrVal('x', EscapedString.to("reserved"))),
        new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
        new Base64String("QSXCR+Q6sek8bf92"),
        4096,
        Seq(
          AttrVal('y', EscapedString.to("yValue")),
          AttrVal('z', EscapedString.to("zValue"))
        )
      )

      renderToString(msg) shouldBe "x=reserved,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096,y=yValue,z=zValue"
    }
  }

  "clientFinalMessageWithoutProofRendering" should {
    "render ClientFinalMessageWithoutProof" in {
      val msg = ClientFinalMessageWithoutProof(
        new Base64String("biws"),
        new PrintableString("fyko"),
        Seq(
          AttrVal('y', EscapedString.to("yValue")),
          AttrVal('z', EscapedString.to("zValue"))
        )
      )

      renderToString(msg) shouldBe "c=biws,r=fyko,y=yValue,z=zValue"
    }
  }

  "internalAuthMessageRendering" should {
    "render InternalAuthMessage" in {
      val msg = InternalAuthMessage(
        ClientFirstMessageBare(
          Some(AttrVal('x', EscapedString.to("reserved"))),
          EscapedString.to("user"),
          new PrintableString("fyko+d2lbbFgONRv9qkxdawL"),
          Seq(
            AttrVal('y', EscapedString.to("yValue")),
            AttrVal('z', EscapedString.to("zValue"))
          )
        ),
        ServerFirstMessage(
          Some(AttrVal('x', EscapedString.to("reserved"))),
          new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
          new Base64String("QSXCR+Q6sek8bf92"),
          4096,
          Seq(
            AttrVal('y', EscapedString.to("yValue")),
            AttrVal('z', EscapedString.to("zValue"))
          )
        ),
        ClientFinalMessageWithoutProof(
          new Base64String("biws"),
          new PrintableString("fyko"),
          Seq(
            AttrVal('y', EscapedString.to("yValue")),
            AttrVal('z', EscapedString.to("zValue"))
          )
        )
      )

      renderToString(msg) shouldBe "x=reserved,n=user,r=fyko+d2lbbFgONRv9qkxdawL,y=yValue,z=zValue,x=reserved,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096,y=yValue,z=zValue,c=biws,r=fyko,y=yValue,z=zValue"
    }
  }

  "clientFinalMessageRendering" should {
    "render ClientFinalMessage" in {
      val msg = ClientFinalMessage(
        ClientFinalMessageWithoutProof(
          new Base64String("biws"),
          new PrintableString("fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"),
          Seq.empty
        ),
        new Base64String("v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=")
      )

      renderToString(msg) shouldBe "c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts="
    }
  }
}
