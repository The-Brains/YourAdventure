package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.Universe
import thebrains.youradventure.FPTerminalIO.Renderer
import thebrains.youradventure.Game.GameStatus
import thebrains.youradventure.Utils.Err

import scala.util.Random

object FGame {
  def apply(
    renderer: Maybe[Renderer] = Maybe.empty,
    universe: Maybe[Universe] = Maybe.empty
  )(
    implicit r: Random
  ): IO[Err, GameStatus] = {
    universe match {
      case Maybe.Just(u) =>
        IO.sync(
          GameStatus.apply(
            renderer getOrElse new Renderer(),
            universe = u
          )
        )
      case Maybe.Empty() =>
        for {
          u <- FUniverse()
        } yield {
          GameStatus.apply(renderer getOrElse new Renderer(), u)
        }
    }
  }
}
