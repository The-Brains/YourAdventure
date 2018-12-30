package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import thebrains.youradventure.Adventure.Location
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FLocation {
  private val DefaultNameLength:        Int = 5
  private val DefaultDescriptionLength: Int = 144

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
