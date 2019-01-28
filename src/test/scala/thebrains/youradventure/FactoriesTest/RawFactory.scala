package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine
import DefaultValues.DefaultListMaxLength
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

  def getList[A](
    length:  Maybe[Int],
    list:    Maybe[List[A]],
    factory: Int => A
  )(
    implicit r: Random
  ): List[A] = {
    list.getOrElse(
      (0 until (length getOrElse RandomMachine.getInt(1, DefaultListMaxLength)))
        .map(factory)
        .toList
    )
  }
}
