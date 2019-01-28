package thebrains.youradventure.FactoriesTest.AttributePack

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.{AttributeCollection, PlayerAttribute}
import thebrains.youradventure.FactoriesTest.DefaultValues._
import thebrains.youradventure.FactoriesTest.RawFactory
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FAttributeCollection {
  def apply(
    lengthAttribute: Maybe[Int] = Maybe.empty,
    attributes:      Maybe[List[PlayerAttribute]] = Maybe.empty
  )(
    implicit r: Random
  ): AttributeCollection = {
    AttributeCollection(
      attributes = RawFactory.getList(
        lengthAttribute,
        attributes,
        _ => FAttribute().toPlayerAttribute(RandomMachine.getInt(1, 10))
      ): _*
    )
  }
}
