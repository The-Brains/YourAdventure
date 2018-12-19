package thebrains.youradventure.TerminalUIPack

import scalaz.Maybe.Just
import scalaz._
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.{Error, Player, Step}

class Renderer(tp: TerminalPrint) {

  def display(error: Error): Maybe[(TerminalPrint, DisplayMessage)] = {
    Just(
      tp,
      TerminalMessageBuilder
        .start()
        .addEmptyLine
        .addLine(error.getCapitalizeName)
        .addLine(error.getDescription)
        .complete()
    )
  }

  def display(
    step:   Step,
    player: Maybe[Player]
  ): Maybe[(TerminalPrint, Either[DisplayQuestion, DisplayMessage])] = {
    Just(
      tp,
      display {
        TerminalMessageBuilder
          .start()
          .addEmptyLine
          .addLine(s"--- ${step.getCapitalizeName} ---")
          .addLine(step.getDescription)
          .addEmptyLine
      }(step.getActions(player))
    )
  }

  private def display(
    buffer: TerminalMessageBuilder.MessageAssembly
  )(
    actions: ActionCollection
  ): Either[DisplayQuestion, DisplayMessage] = {
    actions.getIndexedActions
      .foldLeft(buffer) { case (b, (i, a)) => b ++ display(i, a) }
      .addEmptyLine
      .finishWithQuestion(actions.getQuestion)
  }

  private def display(
    id:     Int,
    action: Action
  ): TerminalMessageBuilder.MessageAssembly = {
    TerminalMessageBuilder
      .start()
      .addLine(s"$id - ${action.name} - ${action.description}")
  }
}

object Renderer {
  def apply(tp: TerminalPrint): Renderer = new Renderer(tp)
}
