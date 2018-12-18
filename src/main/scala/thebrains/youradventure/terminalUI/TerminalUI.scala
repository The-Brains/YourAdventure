package thebrains.youradventure.terminalUI

import thebrains.youradventure.Adventure.PlayerBuilder.PlayerWithName
import thebrains.youradventure.Adventure._
import thebrains.youradventure.terminalUI.Command.TerminalCommand

import scala.io.StdIn

class TerminalUI {
  private val tp: TerminalPrint = new TerminalPrint()

  private def print(step: Step): Unit = {
    tp.printToConsole(step.name)
    tp.printToConsole(step.description)
    step.availableActions.zipWithIndex.foreach { case (a, i) =>
      tp.printToConsole(s"-$i- ${a.name} - ${a.description}")
    }
  }

  private def createCharacter(player: PlayerWithName): Player = {
    tp.printToConsole("Enter Race: ")
    val race = StdIn.readLine()
    player.selectRace(race) match {
      case Right(p) => p
      case Left(error) =>
        tp.printToConsole(error.toDisplay)
        createCharacter(player)
    }
  }

  private def createCharacter(player: Option[Player]): Player = {
    player match {
      case Some(p) => p
      case None =>
        val name = tp.askText("Enter Name: ")
        createCharacter(PlayerBuilder.create(name))
    }
  }

  def run(game: GameStatus, command: Option[TerminalCommand]): GameStatus = {
    (game, command) match {
      case (GameStatus(None, None), None) =>
        tp.printToConsole("Create Character: ")
        val player = createCharacter(None)
        run(GameStatus(None, Some(player)), None)
      case (GameStatus(Some(step), player), None) =>
        print(step)
        val ln = tp.askText("Which option: ")
        run(GameStatus(Some(step), player), Some(TerminalCommand(ln, "")))
      case (g, Some(c)) =>
        run(g.consume(c), None)
      case _ =>
        tp.printToConsole("Terminate")
        game
    }
  }
}
