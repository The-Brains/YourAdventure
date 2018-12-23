package thebrains.youradventure.Adventure.BodyPack

import io.circe.{Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._
import scalaz.Maybe
import thebrains.youradventure.Adventure.Equipment
import thebrains.youradventure.Utils.Error

case class PlayerBodyPart(
  bodyPart:  BodyPart,
  equipment: Maybe[Equipment]
) {

  implicit private val jsonEncoder: Encoder[PlayerBodyPart] =
    Encoder.forProduct2[PlayerBodyPart, Json, Maybe[Json]]("name", "equipment") {
      case PlayerBodyPart(b, e) =>
        (b.encoded, e.map(_.encoded))
    }

  def encoded: Json = this.asJson

  def canEquip(equipment: Equipment): Boolean = {
    bodyPart samePart equipment.bodyPart
  }

  override def toString: String = this.asJson.noSpaces

  def equip(equipment: Equipment): Either[Error, PlayerBodyPart] = {
    if (canEquip(equipment)) {
      Right(this.copy(equipment = Maybe.Just(equipment)))
    } else {
      Left(
        Error(
          "Wrong emplacement for equipment",
          s"The equipment ${equipment.toString} cannot be equipped on " +
            s"emplacement ${this.bodyPart.toString}"
        )
      )
    }

  }
}
