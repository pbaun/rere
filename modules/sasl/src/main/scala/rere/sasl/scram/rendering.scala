package rere.sasl.scram

import rere.sasl._
import rere.sasl.scram.messages._
import rere.sasl.util._

object rendering {

  /**
    * Util classes
    */
  implicit val escapedStringRendering: Rendering[EscapedString] = new Rendering[EscapedString] {
    override def render[R <: Renderer](r: R, str: EscapedString): R = {
      r ~~ str.str
    }
  }

  implicit val noCommaStringRendering: Rendering[NoCommaString] = new Rendering[NoCommaString] {
    override def render[R <: Renderer](r: R, str: NoCommaString): R = {
      r ~~ str.str
    }
  }

  implicit val printableStringRendering: Rendering[PrintableString] = new Rendering[PrintableString] {
    override def render[R <: Renderer](r: R, str: PrintableString): R = {
      r ~~ str.str
    }
  }

  implicit val base64StringRendering: Rendering[Base64String] = new Rendering[Base64String] {
    override def render[R <: Renderer](r: R, str: Base64String): R = {
      r ~~ str.str
    }
  }

  implicit val printableAndSafeRendering: Rendering[PrintableAndSafe] = new Rendering[PrintableAndSafe] {
    override def render[R <: Renderer](r: R, strImpl: PrintableAndSafe): R = {
      strImpl match {
        case printable: PrintableString => printableStringRendering.render(r, printable)
        case base64: Base64String => base64StringRendering.render(r, base64)
      }
    }
  }

  implicit val safeStringRendering: Rendering[SafeString] = new Rendering[SafeString] {
    override def render[R <: Renderer](r: R, strImpl: SafeString): R = {
      strImpl match {
        case esc: EscapedString => escapedStringRendering.render(r, esc)
        case noComma: NoCommaString => noCommaStringRendering.render(r, noComma)
        case printable: PrintableAndSafe => printableAndSafeRendering.render(r, printable)
      }
    }
  }

  /**
    * GS2
    */
  implicit val channelBindingFlagRendering: Rendering[gs2.ChannelBindingFlag] = new Rendering[gs2.ChannelBindingFlag] {

    import gs2.ChannelBindingFlag._

    override def render[R <: Renderer](r: R, flag: gs2.ChannelBindingFlag): R = {
      flag match {
        case NotSupports => r ~~ "n"
        case SupportsButNotUsed => r ~~ "y"
        case SupportsAndUsed(bindingName) => r ~~ "p=" ~~ bindingName
      }
    }
  }

  implicit val headerRendering: Rendering[gs2.Header] = new Rendering[gs2.Header] {
    override def render[R <: Renderer](r: R, header: gs2.Header): R = {
      r ~~ header.channelBinding ~~ ","
      if (header.authId.nonEmpty) { r ~~ "a=" ~~ header.authId }
      r ~~ ","
    }
  }

  /**
    * SCRAM
    */
  implicit val attrValRendering: Rendering[AttrVal] = new Rendering[AttrVal] {
    override def render[R <: Renderer](r: R, attrVal: AttrVal): R = {
      r ~~ attrVal.attribute.toString ~~ "=" ~~ attrVal.value
    }
  }

  implicit val extensionsRendering: Rendering[Seq[AttrVal]] = new Rendering[Seq[AttrVal]] {
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

  implicit val clientFirstMessageBareRendering: Rendering[ClientFirstMessageBare] = {
    new Rendering[ClientFirstMessageBare] {
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

  implicit val clientFirstMessageRendering: Rendering[ClientFirstMessage] = new Rendering[ClientFirstMessage] {
    override def render[R <: Renderer](r: R, msg: ClientFirstMessage): R = {
      r ~~ msg.header ~~ msg.bare
    }
  }

  implicit val serverFirstMessageRendering: Rendering[ServerFirstMessage] = new Rendering[ServerFirstMessage] {
    override def render[R <: Renderer](r: R, msg: ServerFirstMessage): R = {
      if (msg.reserved.nonEmpty) { r ~~ msg.reserved ~~ "," }
      r ~~ "r=" ~~ msg.serverNonce ~~ ","
      r ~~ "s=" ~~ msg.salt ~~ ","
      r ~~ "i=" ~~ msg.iterationCount.toString
      if (msg.extensions.nonEmpty) r ~~ "," ~~ msg.extensions else r
    }
  }

  implicit val clientFinalMessageWithoutProofRendering: Rendering[ClientFinalMessageWithoutProof] = {
    new Rendering[ClientFinalMessageWithoutProof] {
      override def render[R <: Renderer](r: R, msg: ClientFinalMessageWithoutProof): R = {
        r ~~ "c=" ~~ msg.channelBinding ~~ ","
        r ~~ "r=" ~~ msg.nonce
        if (msg.extensions.nonEmpty) r ~~ "," ~~ msg.extensions else r
      }
    }
  }

  implicit val internalAuthMessageRendering: Rendering[InternalAuthMessage] = new Rendering[InternalAuthMessage] {
    override def render[R <: Renderer](r: R, auth: InternalAuthMessage): R = {
      r ~~ auth.clientFirstMessageBare ~~ ","
      r ~~ auth.serverFirstMessage ~~ ","
      r ~~ auth.clientFinalMessageWithoutProof
    }
  }

  implicit val clientFinalMessageRendering: Rendering[ClientFinalMessage] = new Rendering[ClientFinalMessage] {
    override def render[R <: Renderer](r: R, msg: ClientFinalMessage): R = {
      r ~~ msg.bare ~~ ","
      r ~~ "p=" ~~ msg.proof
    }
  }

}
