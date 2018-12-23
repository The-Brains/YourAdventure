package thebrains.youradventure.FPTerminalIO

import scalaz._
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.PlayerBuilder.PlayerWithName
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils.Error

class Renderer() {
  def display(p: Maybe[PlayerTrait]): TerminalMessage = {
    p match {
      case Maybe.Empty() => displayEmptyPlayerQuestion
      case Maybe.Just(p: PlayerWithName) => display(p)
    }
  }

  private def displayEmptyPlayerQuestion: TerminalMessage = {
    TerminalMessageBuilder
      .start()
      .makeQuestion(PlayerBuilder.NameQuestion)
  }

  private def display(p: PlayerBuilder.PlayerWithName): TerminalMessage = {
    TerminalMessageBuilder
      .start()
      .makeQuestion(PlayerBuilder.RaceQuestion)
  }

  def display(txt: String): IO[Nothing, TerminalMessage] = {
    IO.sync(
      TerminalMessageBuilder
        .start()
        .addLine(txt)
        .complete()
    )
  }

  def display(error: Error): IO[Nothing, TerminalMessage] = {
    IO.sync(
      TerminalMessageBuilder
        .start()
        .addLine(error.getCapitalizeName)
        .addLine(error.getDescription)
        .complete()
    )
  }

  def displayEmptyLine: IO[Nothing, TerminalMessage] = {
    IO.sync(TerminalMessageBuilder.start().complete())
  }

  def display(
    step:   Step,
    player: Maybe[Player]
  ): IO[Error, TerminalMessage] = {
    for {
      actions <- step.getActions(player)
    } yield {
      display(
        TerminalMessageBuilder
          .start()
          .addLine(s"--- ${step.getCapitalizeName} ---")
          .addLine(step.getDescription)
          .addEmptyLine,
        actions
      )
    }
  }

  private def display(
    buffer:  TerminalMessageBuilder.MessageAssembly,
    actions: ActionCollection
  ): TerminalMessage = {
    actions.getIndexedActions
      .foldLeft(buffer) {
        case (b, (i, a)) => display(b, i, a)
      }
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
  def apply(): Renderer = new Renderer()
}
