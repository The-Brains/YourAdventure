package thebrains.youradventure.FactoriesTest.TransformationPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FTransformationCollection {
  private val DefaultMaxActionLength: Int = 5

  def apply(
    lengthTransformation: Maybe[Int] = Maybe.empty,
    transformations:      Maybe[List[Transformation]] = Maybe.empty
  )(
    implicit r: Random
  ): TransformationCollection = {
    TransformationCollection(
      transformations = transformations.getOrElse(
        (0 until (lengthTransformation getOrElse RandomMachine.getInt(1, DefaultMaxActionLength)))
          .map(_ => FTransformation())
          .toList
      ): _*
    )
  }
}
