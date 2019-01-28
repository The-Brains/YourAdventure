package thebrains.youradventure.FactoriesTest.BodyPartPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.BodyPack.{PlayerBodyCollection, PlayerBodyPart}
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine
import thebrains.youradventure.FactoriesTest.DefaultValues._
import thebrains.youradventure.FactoriesTest.RawFactory

import scala.util.Random

object FPlayerBodyCollection {
  def apply(
    lengthBodyParts: Maybe[Int] = Maybe.empty,
    bodyParts:       Maybe[List[PlayerBodyPart]] = Maybe.empty
  )(
    implicit r: Random
  ): PlayerBodyCollection = PlayerBodyCollection(
    RawFactory.getList(lengthBodyParts, bodyParts, _ => FPlayerBodyPart()): _*
  )
}
