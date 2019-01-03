package thebrains.youradventure.Adventure.BodyPack

import io.circe.syntax._
import io.circe.{Encoder, Json}
import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Adventure.Equipment
import thebrains.youradventure.Utils.Error
import thebrains.youradventure.Utils.ToOption._

class PlayerBodyPart(bodyPart: BodyPart)
    extends AssemblyItemTrait(
      bodyPart.getName,
      bodyPart.getDescription
    ) {

  @transient lazy val getBodyPart: BodyPart = this.bodyPart
  implicit private val jsonEncoder: Encoder[PlayerBodyPart] =
    Encoder.forProduct1[PlayerBodyPart, Json]("name") {
      case PlayerBodyPart(b) => b.encoded
    }

  override def encoded: Json = this.asJson

  def canEquip(equipment: Equipment): Boolean = {
    bodyPart samePart equipment.bodyPart
  }

  def isWearing: Boolean = false

  override def toString: String = encoded.noSpaces

  def equip(equipment: Equipment): IO[Error, PlayerBodyPartEquipped] = {
    if (canEquip(equipment)) {
      IO.sync(
        new PlayerBodyPartEquipped(
          bodyPart = this.bodyPart,
          equipment = equipment
        )
      )
    } else {
      IO.fail(
        Error(
          "Wrong emplacement for equipment",
          s"The equipment ${equipment.toString} cannot be equipped on " +
            s"emplacement ${bodyPart.toString}"
        )
      )
    }
  }
}

class PlayerBodyPartEquipped(
  bodyPart:  BodyPart,
  equipment: Equipment
) extends PlayerBodyPart(bodyPart) {
  @transient lazy override val isWearing: Boolean = true
  @transient lazy val getEquipment:       Equipment = this.equipment
  implicit private val jsonEncoder: Encoder[PlayerBodyPartEquipped] =
    Encoder.forProduct2[PlayerBodyPartEquipped, Json, Json]("name", "equipment") {
      case PlayerBodyPartEquipped(b, e) =>
        (b.encoded, e.encoded)
    }

  override def encoded: Json = this.asJson

}

object PlayerBodyPart {
  def unapply(arg: PlayerBodyPart): Some[BodyPart] = {
    arg.getBodyPart.some
  }

  def apply(bodyPart: BodyPart): PlayerBodyPart = new PlayerBodyPart(bodyPart)
}

object PlayerBodyPartEquipped {
  def unapply(arg: PlayerBodyPartEquipped): Some[(BodyPart, Equipment)] = {
    (arg.getBodyPart, arg.getEquipment).some
  }

  def apply(
    bodyPart:  BodyPart,
    equipment: Equipment
  ): IO[Error, PlayerBodyPartEquipped] = {
    PlayerBodyPart(bodyPart).equip(equipment)
  }
}
