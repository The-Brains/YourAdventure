package thebrains.youradventure.FactoriesTest.Utils

import scalaz.Maybe
import thebrains.youradventure.Utils.ToOption._
import scala.util.Random

private[FactoriesTest] object RandomMachine {
  private val HundredPercent: Int = 100
  private val Dictionary:     List[String] = (('a' to 'z') :+ ' ').map(_.toString).toList

  def getString(length: Int = 10)(implicit r: Random): String = {
    (0 to length)
      .map(_ => getFrom(Dictionary))
      .mkString("")
  }

  def getFrom[T](dictionary: List[T])(implicit r: Random): T = {
    dictionary(r.nextInt(dictionary.length))
  }

  def getBoolean(implicit r: Random): Boolean = {
    r.nextBoolean
  }

  def getBoolean(percent: Int)(implicit r: Random): Boolean = {
    getInt(min = 0, max = HundredPercent) <= percent
  }

  def getInt(
    min: Int,
    max: Int
  )(
    implicit r: Random
  ): Int = {
    val goodMin = Math.min(max, min)
    val goodMax = Math.max(max, min)
    r.nextInt(goodMax - goodMin) + goodMin
  }

  def getList(
    length: Int,
    min:    Int,
    max:    Int
  )(
    implicit r: Random
  ): List[Int] = {
    (0 until length).map(_ => getInt(min, max)).toList
  }

  def getDouble(max: Double)(implicit r: Random): Double = {
    r.nextDouble() * max
  }

  def getDouble(
    min: Double,
    max: Double
  )(
    implicit r: Random
  ): Double = {
    val goodMin = Math.min(max, min)
    val goodMax = Math.max(max, min)
    getDouble(goodMax - goodMin) + goodMin
  }

  def getEither[A, B](
    left:  => A,
    right: => B
  )(
    implicit r: Random
  ): Either[A, B] = {
    if (getBoolean) {
      Right(right)
    } else {
      Left(left)
    }
  }

  def getMaybe[A](value: => A)(implicit r: Random): Maybe[A] = {
    if (getBoolean) {
      value.just
    } else {
      Maybe.empty
    }
  }
}
