package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FactoriesTest.DefaultValues.DefaultNameLength
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FPlayer {
  def apply(
    name: Maybe[String] = Maybe.empty,
    race: Maybe[Race] = Maybe.empty
  )(
    implicit r: Random
  ): IO[Nothing, Player] = {
    PlayerBuilder
      .create(name = name getOrElse RandomMachine.getString(DefaultNameLength))
      .map(_.selectRace(race getOrElse RandomMachine.getFrom(Races.AllRaces)))
  }
}
