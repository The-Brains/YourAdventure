package thebrains.youradventure.Adventure.BodyPack

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Adventure.CollectionPack.ListImplicits._
import thebrains.youradventure.Adventure.Equipment
import thebrains.youradventure.Utils.Error
import thebrains.youradventure.Utils.ToOption._

class PlayerBodyCollection(bodyParts: List[PlayerBodyPart])
    extends AssemblyTrait[PlayerBodyCollection, PlayerBodyPart](bodyParts) {
  override protected def wrap(items: PlayerBodyPart*): PlayerBodyCollection = {
    new PlayerBodyCollection(items.toList)
  }

  @transient lazy val equipments: List[Equipment] = bodyParts.flatMap {
    case PlayerBodyPartEquipped(_, e) => e.some
    case _: PlayerBodyPart => None
  }

  override protected def empty: PlayerBodyCollection = PlayerBodyCollection.Empty

  /**
    * Protected for test
    */
  protected def getCanEquip(equipment: Equipment): List[PlayerBodyPart] = {
    bodyParts.filter(_.canEquip(equipment))
  }

  /**
    * Protected for test
    */
  protected def getCanEquipAndEmpty(equipment: Equipment): List[PlayerBodyPart] = {
    getCanEquip(equipment).filterNot(_.isWearing)
  }

  def equip(equipment: Equipment): IO[Error, PlayerBodyCollection] = {
    val validBodyPart = getCanEquip(equipment)
    validBodyPart match {
      case parts if parts.isEmpty =>
        IO.fail(
          Error(
            "Not valid body part",
            s"There is no valid body part (${equipment.bodyPart.toString}) " +
              s"to equip: ${equipment.toString}"
          )
        )
      case parts if parts.nonEmpty && getCanEquipAndEmpty(equipment).nonEmpty =>
        IO.sequence(
            bodyParts.updateFirstIO[PlayerBodyPart](
              b => b.canEquip(equipment) && !b.isWearing
            )(
              _.equip(equipment)
            )
          )
          .map(new PlayerBodyCollection(_))
      case parts if parts.nonEmpty =>
        IO.sequence(
            bodyParts.updateFirstIO[PlayerBodyPart](_.canEquip(equipment))(_.equip(equipment))
          )
          .map(new PlayerBodyCollection(_))
    }
  }

  def getBodyPart(bodyPart: BodyPart): Maybe[PlayerBodyPart] = {
    this.find(_.getBodyPart === bodyPart)
  }

  def getEquipment(bodyPart: BodyPart): Maybe[Equipment] = {
    this.getBodyPart(bodyPart).flatMap {
      case p: PlayerBodyPartEquipped => p.getEquipment.just
      case _ => Maybe.empty
    }
  }
}

object PlayerBodyCollection {

  final case object Empty extends PlayerBodyCollection(Nil)

  def apply(bodyParts: PlayerBodyPart*): PlayerBodyCollection = {
    new PlayerBodyCollection(bodyParts.toList)
  }

}
