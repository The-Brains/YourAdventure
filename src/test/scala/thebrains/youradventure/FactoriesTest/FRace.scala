package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.AttributeCollection
import thebrains.youradventure.Adventure.BodyPack.BodyCollection
import thebrains.youradventure.Adventure.{CompoundAttributes, Race}
import thebrains.youradventure.FactoriesTest.AttributePack.FAttributeCollection
import thebrains.youradventure.FactoriesTest.BodyPartPack.FPlayerBodyCollection
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine
import thebrains.youradventure.FactoriesTest.DefaultValues._

import scala.util.Random

object FRace {
  def apply(
    name:                Maybe[String] = Maybe.empty,
    description:         Maybe[String] = Maybe.empty,
    inputBaseAttributes: Maybe[AttributeCollection] = Maybe.empty,
    compoundAttributes:  Maybe[Set[CompoundAttributes]] = Maybe.empty,
    inputBodyParts:      Maybe[BodyCollection] = Maybe.empty
  )(
    implicit r: Random
  ): Race = {
    new Race(
      name = name getOrElse RandomMachine.getString(DefaultNameLength),
      description = description getOrElse RandomMachine.getString(DefaultDescriptionLength),
      inputBaseAttributes = inputBaseAttributes getOrElse FAttributeCollection(),
      compoundAttributes = compoundAttributes getOrElse Set(),
      inputBodyParts =
        inputBodyParts getOrElse BodyCollection(FPlayerBodyCollection().outMap(_.getBodyPart): _*)
    )
  }
}
