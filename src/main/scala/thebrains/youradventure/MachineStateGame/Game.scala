package thebrains.youradventure.MachineStateGame

import scalaz.Maybe
import thebrains.youradventure.Adventure.{Player, PlayerBuilder}
import thebrains.youradventure.TerminalUIPack.GameStatus
import thebrains.youradventure.Utils.Error

case class Game()

object Game {

  def update(i: Maybe[Input])(g: GameStatus): Either[Error, GameStatus] = {
    (i, g) match {
      case (Maybe.Empty(), GameStatus(_, _, _, Maybe.Empty())) =>
        // Create new character
        g.withPlayer(updatePlayer(Maybe.empty)(Maybe.empty))
    }
  }

  def updatePlayer(
    i: Maybe[Input]
  )(
    p: Maybe[Either[PlayerBuilder.PlayerWithName, Player]]
  ): Either[Error, Player] = {
    (i, p) match {
      case (Maybe.Empty(), Maybe.Empty()) =>
        // ask for name
        val input: Maybe.Just[Input] = ???
        updatePlayer(input)(p)

      case (Maybe.Just(Input(text)), Maybe.Empty()) =>
        updatePlayer(Maybe.empty)(Maybe.Just(Left(PlayerBuilder.create(text))))

      case (Maybe.Empty(), Maybe.Just(Left(_))) =>
        // ask for race
        val input: Maybe.Just[Input] = ???
        updatePlayer(input)(p)

      case (Maybe.Just(Input(text)), Maybe.Just(Left(player))) =>
        player.selectRace(text) match {
          case Left(error) =>
            if (error.isFatal) {
              Left(error)
            } else {
              updatePlayer(Maybe.empty)(Maybe.Just(Left(player)))
            }
          case Right(pl) => updatePlayer(Maybe.empty)(Maybe.Just(Right(pl)))
        }

      case (_, Maybe.Just(Right(player))) =>
        Right(player)

      case _ =>
        Left(
          Error(
            "Failed to create player",
            s"Unknown cases with input: $i and player: $p"
          )
        )
    }
  }
}
