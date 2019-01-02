package thebrains.youradventure.FPTerminalIO

import io.circe.{Encoder, Json}
import scalaz.Maybe
import io.circe.generic.auto._
import io.circe.syntax._
import thebrains.youradventure.Utils.ToOption._

sealed trait TerminalMessage

case class Line(
  content:   String,
  cutLength: Boolean = true
)

final class MessageToDisplay(
  messages:   List[Line],
  isQuestion: Boolean
) extends TerminalMessage {
  lazy val question:      Maybe[Line] = if (isQuestion) messages.last.just else Maybe.empty
  lazy val messageToShow: Seq[Line] = if (isQuestion) messages.dropRight(1) else messages

  implicit private val jsonEncoder: Encoder[MessageToDisplay] =
    Encoder.forProduct2[MessageToDisplay, Seq[Line], Maybe[Line]]("question", "question") { msg =>
      (msg.messageToShow, msg.question)
    }

  @transient lazy val encoded: Json = this.asJson

  override def toString: String = encoded.noSpaces
}

object TerminalMessageBuilder {

  case class MessageAssembly(messages: List[Line]) {
    def addLine(
      text:      String,
      cutLength: Boolean = true
    ): MessageAssembly = {
      this.copy(messages = this.messages :+ Line(text, cutLength))
    }

    def addLineMaybe(
      text:      Maybe[String],
      cutLength: Boolean = true
    ): MessageAssembly = {
      text match {
        case Maybe.Just(line) => this.addLine(line, cutLength)
        case Maybe.Empty()    => this
      }
    }

    def addEmptyLine: MessageAssembly = {
      this.addLine("")
    }

    def makeQuestion(question: String): MessageToDisplay = {
      finishWithQuestion(question.just)
    }

    def finishWithQuestion(text: Maybe[String]): MessageToDisplay = {
      text match {
        case Maybe.Just(question) =>
          new MessageToDisplay(messages = this.messages :+ Line(question), isQuestion = true)
        case Maybe.Empty() => complete()
      }
    }

    def complete(): MessageToDisplay = {
      new MessageToDisplay(messages, isQuestion = false)
    }

    def ++(other: MessageAssembly): MessageAssembly = {
      this.copy(messages = this.messages ++ other.messages)
    }
  }

  def start(): MessageAssembly = {
    MessageAssembly(Nil).addEmptyLine
  }

  final case object EmptyMessage extends TerminalMessage

}
