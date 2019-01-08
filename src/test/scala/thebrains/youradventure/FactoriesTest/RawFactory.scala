package thebrains.youradventure.FactoriesTest

import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object RawFactory {
  def getString(length: Int = DefaultValues.DefaultNameLength)(implicit r: Random): String = {
    RandomMachine.getString(length)
  }

  def getInt(
    min: Int = 0,
    max: Int = 10
  )(
    implicit r: Random
  ): Int = {
    RandomMachine.getInt(min, max)
  }
}
