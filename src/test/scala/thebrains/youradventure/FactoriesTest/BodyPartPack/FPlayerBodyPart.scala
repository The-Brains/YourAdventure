package thebrains.youradventure.FactoriesTest.BodyPartPack

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.BodyPack._
import thebrains.youradventure.Adventure.Equipment
import thebrains.youradventure.FactoriesTest.FEquipment
import thebrains.youradventure.Utils

import scala.util.Random

object FPlayerBodyPart {
  def apply(bodyPart: Maybe[BodyPart] = Maybe.empty)(implicit r: Random): PlayerBodyPart = {
    PlayerBodyPart(
      bodyPart = bodyPart getOrElse FBodyPart()
    )
  }
}

object FPlayerBodyPartEquipped {
  def apply(
    bodyPart:  Maybe[BodyPart] = Maybe.empty,
    equipment: Maybe[Equipment] = Maybe.empty
  )(
    implicit r: Random
  ): IO[Utils.Error, PlayerBodyPartEquipped] = {
    PlayerBodyPartEquipped(
      bodyPart = bodyPart getOrElse FBodyPart(),
      equipment = equipment getOrElse FEquipment()
    )
  }
}
