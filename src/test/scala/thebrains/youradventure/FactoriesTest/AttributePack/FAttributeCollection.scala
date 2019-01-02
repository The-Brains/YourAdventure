package thebrains.youradventure.FactoriesTest.AttributePack

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.{AttributeCollection, PlayerAttribute}
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random
import thebrains.youradventure.FactoriesTest.DefaultValues._

object FAttributeCollection {
  def apply(
    lengthAttribute: Maybe[Int] = Maybe.empty,
    attributes:      Maybe[List[PlayerAttribute]] = Maybe.empty
  )(
    implicit r: Random
  ): AttributeCollection = {
    AttributeCollection(
      attributes = attributes.getOrElse(
        (0 until (lengthAttribute getOrElse RandomMachine.getInt(1, DefaultListMaxLength)))
          .map(_ => FAttribute().toPlayerAttribute(RandomMachine.getInt(1, 10)))
          .toList
      ): _*
    )
  }
}
