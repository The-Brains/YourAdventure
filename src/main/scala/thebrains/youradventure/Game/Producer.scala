package thebrains.youradventure.Game

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.Player
import thebrains.youradventure.FPTerminalIO.TerminalMessage
import thebrains.youradventure.Utils.{Err, ErrorIO}
import thebrains.youradventure.Utils.ToOption._

private[Game] class Producer(game: GameStatus) {

  def getNextMessage: IO[Err, TerminalMessage] = {
    game match {
      case GameStatus(_, Maybe.Just(error), _, _, _, r) =>
        // An error should be displayed
        r.display(error)
      case GameStatus(_, _, Maybe.Empty(), _, _, r) =>
        // No step
        r.displayEndGame
      case GameStatus(_, _, Maybe.Just(_), Maybe.Just(_), Maybe.Just(_: Player), r) =>
        // When step with action
        r.emptyMessage
      case GameStatus(_, _, Maybe.Just(s), Maybe.Empty(), Maybe.Just(p: Player), r) =>
        // When step but no action
        r.display(s, p.just)
      case GameStatus(_, _, _, _, p, r) =>
        // Display player / Create player
        r.display(p)
      case _ =>
        ErrorIO("End State", "No more state")
    }
  }
}
