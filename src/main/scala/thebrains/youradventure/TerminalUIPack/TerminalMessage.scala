package thebrains.youradventure.TerminalUIPack

import scalaz.Maybe

abstract class TerminalMessage(
  messages: List[String],
  isQuestion: Boolean
) {
  lazy protected val question: Option[String] = if (isQuestion) Some(messages.last) else None
  lazy protected val messageToShow: Seq[String] =
    if (isQuestion) messages.dropRight(1) else messages

  protected def printToConsole(tp: TerminalPrint): Unit = {
    tp.printToConsole(messageToShow)

  }
}

case class DisplayMessage(messages: List[String])
  extends TerminalMessage(messages, isQuestion = false) {
  def displayWith(tp: TerminalPrint): Unit = {
    printToConsole(tp)
  }
}

case class DisplayQuestion(messages: List[String])
  extends TerminalMessage(messages, isQuestion = true) {

  def displayWithQuestionAnd(
    tp: TerminalPrint,
    answer: Maybe[String] = Maybe.empty
  ): String = {
    printToConsole(tp)
    tp.askText(question.get, answer)
  }
}

object TerminalMessageBuilder {

  case class MessageAssembly(messages: List[String]) {
    def addLine(text: String): MessageAssembly = {
      this.copy(messages = this.messages :+ text)
    }

    def addEmptyLine: MessageAssembly = {
      this.addLine("")
    }

    def finishWithQuestion(text: Maybe[String]): Either[DisplayQuestion, DisplayMessage] = {
      text match {
        case Maybe.Just(question) => Left(DisplayQuestion(messages = this.messages :+ question))
        case Maybe.Empty() => Right(complete())
      }
    }

    def complete(): DisplayMessage = {
      DisplayMessage(messages)
    }

    def ++(other: MessageAssembly): MessageAssembly = {
      this.copy(messages = this.messages ++ other.messages)
    }
  }

  def start(): MessageAssembly = {
    MessageAssembly(Nil)
  }
}
