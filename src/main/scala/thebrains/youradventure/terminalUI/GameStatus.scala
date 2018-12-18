package thebrains.youradventure.terminalUI

import thebrains.youradventure.Adventure.{Player, Step}
import thebrains.youradventure.terminalUI.Command.TerminalCommand

case class GameStatus(
  currentStep: Option[Step],
  player: Option[Player],
) {
  def consume(c: TerminalCommand): GameStatus = {
    this
  }
}
