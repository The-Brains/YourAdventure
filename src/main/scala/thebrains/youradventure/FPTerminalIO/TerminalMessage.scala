package thebrains.youradventure.FPTerminalIO

import scalaz.Maybe

sealed trait TerminalMessage

case class Line(
  content: String,
  cutLength: Boolean = true
)

class MessageToDisplay(
  messages: List[Line],
  isQuestion: Boolean
) extends TerminalMessage {
  lazy val question: Maybe[Line] = if (isQuestion) Maybe.Just(messages.last) else Maybe.empty
  lazy val messageToShow: Seq[Line] =
    if (isQuestion) messages.dropRight(1) else messages
}

object TerminalMessageBuilder {

  case class MessageAssembly(messages: List[Line]) {
    def addLine(text: String, cutLength: Boolean = true): MessageAssembly = {
      this.copy(messages = this.messages :+ Line(text, cutLength))
    }

    def addLineMaybe(text: Maybe[String], cutLength: Boolean = true): MessageAssembly = {
      text match {
        case Maybe.Just(line) => this.addLine(line, cutLength)
        case Maybe.Empty() => this
      }
    }

    def addEmptyLine: MessageAssembly = {
      this.addLine("")
    }

    def makeQuestion(question: String): MessageToDisplay = {
      finishWithQuestion(Maybe.just(question))
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

  case object EmptyMessage extends TerminalMessage

}
