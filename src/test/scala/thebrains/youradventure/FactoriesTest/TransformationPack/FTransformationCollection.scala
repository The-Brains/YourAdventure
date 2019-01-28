package thebrains.youradventure.FactoriesTest.TransformationPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.FactoriesTest.DefaultValues.DefaultListMaxLength
import thebrains.youradventure.FactoriesTest.RawFactory
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FTransformationCollection {
  def apply(
    lengthTransformation: Maybe[Int] = Maybe.empty,
    transformations:      Maybe[List[Transformation]] = Maybe.empty
  )(
    implicit r: Random
  ): TransformationCollection = {
    TransformationCollection(
      transformations =
        RawFactory.getList(lengthTransformation, transformations, _ => FTransformation()): _*
    )
  }
}
