package thebrains.youradventure.FPTerminalIO

import scalaz.Maybe

case class TerminalMessage(
  messages: List[String],
  isQuestion: Boolean
) {
  lazy val question: Maybe[String] = if (isQuestion) Maybe.Just(messages.last) else Maybe.empty
  lazy val messageToShow: Seq[String] =
    if (isQuestion) messages.dropRight(1) else messages
}

object TerminalMessageBuilder {

  case class MessageAssembly(messages: List[String]) {
    def addLine(text: String): MessageAssembly = {
      this.copy(messages = this.messages :+ text)
    }

    def addEmptyLine: MessageAssembly = {
      this.addLine("")
    }

    def makeQuestion(question: String): TerminalMessage = {
      finishWithQuestion(Maybe.just(question))
    }

    def finishWithQuestion(text: Maybe[String]): TerminalMessage = {
      text match {
        case Maybe.Just(question) => TerminalMessage(messages = this.messages :+ question,
          isQuestion = true)
        case Maybe.Empty() => complete()
      }
    }

    def complete(): TerminalMessage = {
      TerminalMessage(messages, isQuestion = false)
    }

    def ++(other: MessageAssembly): MessageAssembly = {
      this.copy(messages = this.messages ++ other.messages)
    }
  }

  def start(): MessageAssembly = {
    MessageAssembly(Nil).addEmptyLine
  }
}
