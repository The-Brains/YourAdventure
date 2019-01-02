package thebrains.youradventure.FactoriesTest.BodyPartPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.BodyPack.BodyPart
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine
import thebrains.youradventure.FactoriesTest.DefaultValues._
import scala.util.Random

object FBodyPart {
  def apply(
    name:        Maybe[String] = Maybe.empty,
    description: Maybe[String] = Maybe.empty,
    descriptor:  Maybe[Maybe[String]] = Maybe.empty
  )(
    implicit r: Random
  ): BodyPart = {
    BodyPart(
      name = name getOrElse RandomMachine.getString(DefaultNameLength),
      description = description getOrElse RandomMachine.getString(DefaultDescriptionLength),
      descriptor = descriptor getOrElse RandomMachine.getMaybe(RandomMachine.getString())
    )
  }
}
