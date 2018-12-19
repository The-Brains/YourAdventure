package thebrains.youradventure.TerminalUIPack

import thebrains.youradventure.Adventure._
import thebrains.youradventure.TerminalUIPack.Command.TerminalCommand

case class GameStatus(
  universe:    Universe,
  currentStep: Option[Step],
  player:      Option[Player]
) {
  def consume(c: TerminalCommand): GameStatus = {
    this
  }
}
