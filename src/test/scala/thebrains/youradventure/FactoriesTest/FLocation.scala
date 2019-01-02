package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import thebrains.youradventure.Adventure.Location
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine
import thebrains.youradventure.FactoriesTest.DefaultValues._
import scala.util.Random

object FLocation {
  def apply(
    name:           Maybe[String] = Maybe.empty,
    description:    Maybe[String] = Maybe.empty,
    parentLocation: Maybe[Maybe[Location]] = Maybe.empty
  )(
    implicit r: Random
  ): Location = {
    new Location(
      name = name getOrElse RandomMachine.getString(DefaultNameLength),
      description = description getOrElse RandomMachine.getString(DefaultDescriptionLength),
      parentLocation = parentLocation getOrElse RandomMachine.getMaybe(FLocation())
    )
  }
}
