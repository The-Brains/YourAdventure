package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import thebrains.youradventure.Adventure.Consumable
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.FactoriesTest.DefaultValues._
import thebrains.youradventure.FactoriesTest.TransformationPack.{
  FTransformation,
  FTransformationCollection
}
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FConsumable {
  def apply(
    name:        Maybe[String] = Maybe.empty,
    description: Maybe[String] = Maybe.empty,
    modifiers:   Maybe[TransformationCollection] = Maybe.empty
  )(
    implicit r: Random
  ): Consumable = {
    new Consumable(
      name = name getOrElse RandomMachine.getString(DefaultNameLength),
      description = description getOrElse RandomMachine.getString(DefaultDescriptionLength),
      modifiers = modifiers getOrElse FTransformationCollection()
    )
  }
}
