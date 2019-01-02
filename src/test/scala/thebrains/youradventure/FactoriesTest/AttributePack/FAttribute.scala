package thebrains.youradventure.FactoriesTest.AttributePack

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.Attribute
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine
import thebrains.youradventure.FactoriesTest.DefaultValues._
import scala.util.Random

object FAttribute {
  def apply(
    name:        Maybe[String] = Maybe.empty,
    description: Maybe[String] = Maybe.empty
  )(
    implicit r: Random
  ): Attribute = new Attribute(
    name = name getOrElse RandomMachine.getString(DefaultNameLength),
    description = description getOrElse RandomMachine.getString(DefaultDescriptionLength)
  )
}
