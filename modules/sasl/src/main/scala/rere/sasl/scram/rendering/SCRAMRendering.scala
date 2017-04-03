package rere.sasl.scram.rendering

import rere.sasl._
import rere.sasl.scram.messages._
import rere.sasl.util._

trait SCRAMRendering[T] extends Rendering[T]

object SCRAMRendering {

  def apply[T](implicit rendering: SCRAMRendering[T]): SCRAMRendering[T] = rendering

  /**
    * Util classes
    */
  implicit val escapedStringRendering: SCRAMRendering[EscapedString] = {
    new SCRAMRendering[EscapedString] {
      override def render[R <: Renderer](r: R, str: EscapedString): R = {
        r ~~ str.str
      }
    }
  }

  implicit val noCommaStringRendering: SCRAMRendering[NoCommaString] = {
    new SCRAMRendering[NoCommaString] {
      override def render[R <: Renderer](r: R, str: NoCommaString): R = {
        r ~~ str.str
      }
    }
  }

  implicit val printableStringRendering: SCRAMRendering[PrintableString] = {
    new SCRAMRendering[PrintableString] {
      override def render[R <: Renderer](r: R, str: PrintableString): R = {
        r ~~ str.str
      }
    }
  }

  implicit val base64StringRendering: SCRAMRendering[Base64String] = {
    new SCRAMRendering[Base64String] {
      override def render[R <: Renderer](r: R, str: Base64String): R = {
        r ~~ str.str
      }
    }
  }

  implicit val printableAndSafeRendering: SCRAMRendering[PrintableAndSafe] = {
    new SCRAMRendering[PrintableAndSafe] {
      override def render[R <: Renderer](r: R, strImpl: PrintableAndSafe): R = {
        strImpl match {
          case printable: PrintableString => printableStringRendering.render(r, printable)
          case base64: Base64String => base64StringRendering.render(r, base64)
        }
      }
    }
  }

  implicit val safeStringRendering: SCRAMRendering[SafeString] = {
    new SCRAMRendering[SafeString] {
      override def render[R <: Renderer](r: R, strImpl: SafeString): R = {
        strImpl match {
          case esc: EscapedString => escapedStringRendering.render(r, esc)
          case noComma: NoCommaString => noCommaStringRendering.render(r, noComma)
          case printable: PrintableAndSafe => printableAndSafeRendering.render(r, printable)
        }
      }
    }
  }

  /**
    * GS2
    */
  implicit val channelBindingFlagRendering: SCRAMRendering[gs2.ChannelBindingFlag] = {
    new SCRAMRendering[gs2.ChannelBindingFlag] {
      import gs2.ChannelBindingFlag._

      override def render[R <: Renderer](r: R, flag: gs2.ChannelBindingFlag): R = {
        flag match {
          case NotSupports => r ~~ "n"
          case SupportsButNotUsed => r ~~ "y"
          case SupportsAndUsed(bindingName) => r ~~ "p=" ~~ bindingName
        }
      }
    }
  }

  implicit val headerRendering: SCRAMRendering[gs2.Header] = {
    new SCRAMRendering[gs2.Header] {
      override def render[R <: Renderer](r: R, header: gs2.Header): R = {
        r ~~ header.channelBinding ~~ ","
        if (header.authId.nonEmpty) {
          r ~~ "a=" ~~ header.authId
        }
        r ~~ ","
      }
    }
  }

  /**
    * SCRAM
    */
  implicit val attrValRendering: SCRAMRendering[AttrVal] = {
    new SCRAMRendering[AttrVal] {
      override def render[R <: Renderer](r: R, attrVal: AttrVal): R = {
        r ~~ attrVal.attribute.toString ~~ "=" ~~ attrVal.value
      }
    }
  }

  implicit val extensionsRendering: SCRAMRendering[Seq[AttrVal]] = {
    new SCRAMRendering[Seq[AttrVal]] {
      override def render[R <: Renderer](r: R, attrVals: Seq[AttrVal]): R = {
        attrVals match {
          case Nil => r
          case first :: other =>
            r ~~ first
            other foreach (attrVal => r ~~ "," ~~ attrVal)
            r
        }
      }
    }
  }

  implicit val clientFirstMessageBareRendering: SCRAMRendering[ClientFirstMessageBare] = {
    new SCRAMRendering[ClientFirstMessageBare] {
      override def render[R <: Renderer](r: R, bare: ClientFirstMessageBare): R = {
        if (bare.reserved.nonEmpty) { r ~~ bare.reserved ~~ "," }
        r ~~ "n=" ~~ bare.username ~~ ",r=" ~~ bare.clientNonce
        if (bare.extensions.nonEmpty) {
          r ~~ "," ~~ bare.extensions
        }
        r
      }
    }
  }

  implicit val clientFirstMessageRendering: SCRAMRendering[ClientFirstMessage] = {
    new SCRAMRendering[ClientFirstMessage] {
      override def render[R <: Renderer](r: R, msg: ClientFirstMessage): R = {
        r ~~ msg.header ~~ msg.bare
      }
    }
  }

  implicit val serverFirstMessageRendering: SCRAMRendering[ServerFirstMessage] = new SCRAMRendering[ServerFirstMessage] {
    override def render[R <: Renderer](r: R, msg: ServerFirstMessage): R = {
      if (msg.reserved.nonEmpty) { r ~~ msg.reserved ~~ "," }
      r ~~ "r=" ~~ msg.serverNonce ~~ ","
      r ~~ "s=" ~~ msg.salt ~~ ","
      r ~~ "i=" ~~ msg.iterationCount.toString
      if (msg.extensions.nonEmpty) r ~~ "," ~~ msg.extensions else r
    }
  }

  implicit val clientFinalMessageWithoutProofRendering: SCRAMRendering[ClientFinalMessageWithoutProof] = {
    new SCRAMRendering[ClientFinalMessageWithoutProof] {
      override def render[R <: Renderer](r: R, msg: ClientFinalMessageWithoutProof): R = {
        r ~~ "c=" ~~ msg.channelBinding ~~ ","
        r ~~ "r=" ~~ msg.nonce
        if (msg.extensions.nonEmpty) r ~~ "," ~~ msg.extensions else r
      }
    }
  }

  implicit val internalAuthMessageRendering: SCRAMRendering[InternalAuthMessage] = {
    new SCRAMRendering[InternalAuthMessage] {
      override def render[R <: Renderer](r: R, auth: InternalAuthMessage): R = {
        r ~~ auth.clientFirstMessageBare ~~ ","
        r ~~ auth.serverFirstMessage ~~ ","
        r ~~ auth.clientFinalMessageWithoutProof
      }
    }
  }

  implicit val clientFinalMessageRendering: SCRAMRendering[ClientFinalMessage] = {
    new SCRAMRendering[ClientFinalMessage] {
      override def render[R <: Renderer](r: R, msg: ClientFinalMessage): R = {
        r ~~ msg.bare ~~ ","
        r ~~ "p=" ~~ msg.proof
      }
    }
  }

}
