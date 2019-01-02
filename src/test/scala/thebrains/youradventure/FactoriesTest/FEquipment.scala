package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import thebrains.youradventure.Adventure.BodyPack.BodyPart
import thebrains.youradventure.Adventure.Equipment
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.FactoriesTest.BodyPartPack.FBodyPart
import thebrains.youradventure.FactoriesTest.TransformationPack.FTransformationCollection
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FEquipment {
  private val DefaultNameLength:        Int = 5
  private val DefaultDescriptionLength: Int = 144

  def apply(
    name:            Maybe[String] = Maybe.empty,
    description:     Maybe[String] = Maybe.empty,
    bodyPart:        Maybe[BodyPart] = Maybe.empty,
    transformations: Maybe[TransformationCollection] = Maybe.empty
  )(
    implicit r: Random
  ): Equipment = Equipment(
    name = name getOrElse RandomMachine.getString(DefaultNameLength),
    description = description getOrElse RandomMachine.getString(DefaultDescriptionLength),
    bodyPart = bodyPart getOrElse FBodyPart(),
    modifiers = transformations getOrElse FTransformationCollection()
  )
}
