package thebrains.youradventure.TerminalUIPack

import scalaz.Maybe

case class PrintableMessage(
  tm: TerminalMessage,
  tp: TerminalPrint
) {
  def printToConsole(): Unit = {
    tm displayWith tp
  }
}

abstract class TerminalMessage(
  messages:   List[String],
  isQuestion: Boolean
) {
  lazy protected val question: Option[String] = if (isQuestion) Some(messages.last) else None
  lazy protected val messageToShow: Seq[String] =
    if (isQuestion) messages.dropRight(1) else messages

  def displayWith(tp: TerminalPrint): Unit = {
    tp.printToConsole(messageToShow)
  }

  def toPrintableMessage(tp: TerminalPrint): PrintableMessage = {
    PrintableMessage(this, tp)
  }
}

case class DisplayMessage(messages: List[String]) extends TerminalMessage(messages, false)

case class DisplayQuestion(messages: List[String]) extends TerminalMessage(messages, true) {
  override def displayWith(tp: TerminalPrint): Unit = {
    super.displayWith(tp)
    tp.askText(question.get)
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

    def finishWithQuestion(text: Maybe[String]): TerminalMessage = {
      text match {
        case Maybe.Just(question) => DisplayQuestion(messages = this.messages :+ question)
        case Maybe.Empty()        => complete()
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
