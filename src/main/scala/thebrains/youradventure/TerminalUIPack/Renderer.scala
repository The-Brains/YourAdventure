package thebrains.youradventure.TerminalUIPack

import scalaz.Maybe.Just
import scalaz._
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.{Player, PlayerBuilder, Step}
import thebrains.youradventure.Utils.Error

class Renderer(tp: TerminalPrint) {

  def getTP: TerminalPrint = tp

  def displayEmptyPlayer: DisplayQuestion = {
    TerminalMessageBuilder
      .start()
      .makeQuestion(PlayerBuilder.NameQuestion)
  }

  def display(p: PlayerBuilder.PlayerWithName): DisplayQuestion = {
    TerminalMessageBuilder
      .start()
      .makeQuestion(PlayerBuilder.RaceQuestion)
  }

  def display(error: Error): Maybe[(TerminalPrint, DisplayMessage)] = {
    Just(
      tp,
      TerminalMessageBuilder
        .start()
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
      display(
        TerminalMessageBuilder
          .start()
          .addLine(s"--- ${step.getCapitalizeName} ---")
          .addLine(step.getDescription)
          .addEmptyLine,
        step.getActions(player)
      )
    )
  }

  private def display(
    buffer:  TerminalMessageBuilder.MessageAssembly,
    actions: ActionCollection
  ): Either[DisplayQuestion, DisplayMessage] = {
    actions.getIndexedActions
      .foldLeft(buffer) { case (b, (i, a)) => display(b, i, a) }
      .addEmptyLine
      .finishWithQuestion(actions.getQuestion)
  }

  private def display(
    buffer: TerminalMessageBuilder.MessageAssembly,
    id:     Int,
    action: Action
  ): TerminalMessageBuilder.MessageAssembly = {
    buffer
      .addLine(s"$id - ${action.name} - ${action.description}")
  }
}

object Renderer {
  def apply(tp: TerminalPrint): Renderer = new Renderer(tp)
}
