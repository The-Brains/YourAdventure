package thebrains.youradventure.FactoriesTest.TransformationPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.Attribute
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.FactoriesTest.AttributePack.FAttribute
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FTransformation {
  def apply(
    operation: Maybe[FullOperation] = Maybe.empty,
    value:     Maybe[AttributeType] = Maybe.empty,
    attribute: Maybe[Attribute] = Maybe.empty
  )(
    implicit r: Random
  ): Transformation = {
    TransformationBuilder
      .willDo(operation getOrElse RandomMachine.getFrom(TransformationBuilder.AvailableOperations))
      .byValueOf(value getOrElse RandomMachine.getInt(1, 10))
      .onAttribute(attribute getOrElse FAttribute())
  }
}
