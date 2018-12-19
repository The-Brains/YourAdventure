package thebrains.youradventure.TerminalUIPack

import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.Step

class Renderer(tp: TerminalPrint) {

  def display(step: Step): PrintableMessage = {
    display {
      TerminalMessageBuilder
        .start()
        .addEmptyLine
        .addLine(s"--- ${step.getCapitalizeName} ---")
        .addLine(step.getDescription)
        .addEmptyLine
    }(step.getActions)
      .toPrintableMessage(tp)
  }

  def display(
    buffer: TerminalMessageBuilder.MessageAssembly
  )(
    actions: ActionCollection
  ): TerminalMessage = {
    actions.getIndexedActions
      .foldLeft(buffer) { case (b, (i, a)) => b ++ display(i, a) }
      .addEmptyLine
      .finishWithQuestion(actions.getQuestion)
  }

  def display(
    id:     Int,
    action: Action
  ): TerminalMessageBuilder.MessageAssembly = {
    TerminalMessageBuilder
      .start()
      .addLine(s"$id - ${action.name} - ${action.description}")
  }

  def render(message: TerminalMessage): Unit = {
    message.displayWith(tp)
  }
}

object Renderer {
  def apply(tp: TerminalPrint): Renderer = new Renderer(tp)
}
