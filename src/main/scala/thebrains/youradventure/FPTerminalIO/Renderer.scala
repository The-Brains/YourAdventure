package thebrains.youradventure.FPTerminalIO

import scalaz._
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.PlayerBuilder.PlayerWithName
import thebrains.youradventure.Adventure.StepPack.Step
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils.{Error, FatalError}

class Renderer() {
  def display(p: Maybe[PlayerTrait]): IO[Error, MessageToDisplay] = {
    p match {
      case Maybe.Empty() => displayEmptyPlayerQuestion
      case Maybe.Just(p: PlayerWithName) => display(p)
      case _ => display(FatalError("Invalid case", s"No case for Player: '${p.toString}'."))
    }
  }

  private def displayEmptyPlayerQuestion: IO[Error, MessageToDisplay] = {
    IO.sync(TerminalMessageBuilder
      .start()
      .makeQuestion(PlayerBuilder.NameQuestion))
  }

  private def display(p: PlayerBuilder.PlayerWithName): IO[Error, MessageToDisplay] = {
    IO.sync(TerminalMessageBuilder
      .start()
      .makeQuestion(PlayerBuilder.RaceQuestion))
  }

  def display(txt: String): IO[Nothing, MessageToDisplay] = {
    IO.sync(
      TerminalMessageBuilder
        .start()
        .addLine(txt)
        .complete()
    )
  }

  def displayErrorAsMessage(error: Error): IO[Nothing, MessageToDisplay] = {
    IO.sync(
      TerminalMessageBuilder
        .start()
        .addLine(error.getCapitalizeName, cutLength = !error.isFatal)
        .addLine(error.getDescription, cutLength = !error.isFatal)
        .addLineMaybe(error.stackAsString, cutLength = false)
        .complete()
    )
  }

  def display(error: Error): IO[Error, MessageToDisplay] = {
    if (error.isFatal) {
      IO.fail(error)
    } else {
      displayErrorAsMessage(error)
    }
  }

  def displayEndGame: IO[Nothing, MessageToDisplay] = {
    display("Thanks for playing. The End.")
  }

  def displayEmptyLine: IO[Nothing, MessageToDisplay] = {
    IO.sync(TerminalMessageBuilder.start().complete())
  }

  def display(
    step: Step,
    player: Maybe[Player]
  ): IO[Error, MessageToDisplay] = {
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
    buffer: TerminalMessageBuilder.MessageAssembly,
    actions: ActionCollection
  ): MessageToDisplay = {
    actions.getIndexedActions
      .foldLeft(buffer) {
        case (b, (i, a)) => display(b, i, a)
      }
      .addEmptyLine
      .finishWithQuestion(actions.getQuestion)
  }

  private def display(
    buffer: TerminalMessageBuilder.MessageAssembly,
    id: Int,
    action: Action
  ): TerminalMessageBuilder.MessageAssembly = {
    buffer
      .addLine(s"$id - ${action.name} - ${action.description}")
  }

  def emptyMessage: IO[Error, TerminalMessage] = IO.sync(TerminalMessageBuilder.EmptyMessage)

}

object Renderer {
  def apply(): Renderer = new Renderer()
}
