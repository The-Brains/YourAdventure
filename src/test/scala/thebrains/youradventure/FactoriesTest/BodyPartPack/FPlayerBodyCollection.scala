package thebrains.youradventure.FactoriesTest.BodyPartPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.BodyPack.{PlayerBodyCollection, PlayerBodyPart}
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine
import thebrains.youradventure.FactoriesTest.DefaultValues._
import scala.util.Random

object FPlayerBodyCollection {
  def apply(
    lengthBodyParts: Maybe[Int] = Maybe.empty,
    bodyParts:       Maybe[List[PlayerBodyPart]] = Maybe.empty
  )(
    implicit r: Random
  ): PlayerBodyCollection = PlayerBodyCollection(
    bodyParts.getOrElse(
      (0 until (lengthBodyParts getOrElse RandomMachine.getInt(1, DefaultListMaxLength)))
        .map(_ => FPlayerBodyPart())
        .toList
    ): _*
  )
}
