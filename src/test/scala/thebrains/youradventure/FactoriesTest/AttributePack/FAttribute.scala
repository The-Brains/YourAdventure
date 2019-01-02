package thebrains.youradventure.FactoriesTest.AttributePack

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.Attribute
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FAttribute {
  private val DefaultNameLength:        Int = 5
  private val DefaultDescriptionLength: Int = 144

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
