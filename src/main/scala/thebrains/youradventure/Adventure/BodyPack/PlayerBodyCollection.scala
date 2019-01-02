package thebrains.youradventure.Adventure.BodyPack

import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Adventure.Equipment
import thebrains.youradventure.Utils.ToOption._

class PlayerBodyCollection(bodyParts: List[PlayerBodyPart])
    extends AssemblyTrait[PlayerBodyCollection, PlayerBodyPart](bodyParts.toList) {
  override protected def wrap(items: PlayerBodyPart*): PlayerBodyCollection = {
    new PlayerBodyCollection(items.toList)
  }

  @transient lazy val equipments: List[Equipment] = bodyParts.flatMap {
    case PlayerBodyPartEquipped(_, e) => e.some
    case _: PlayerBodyPart => None
  }

  override protected def empty: PlayerBodyCollection = PlayerBodyCollection.Empty
}

object PlayerBodyCollection {

  final case object Empty extends PlayerBodyCollection(Nil)

  def apply(bodyParts: PlayerBodyPart*): PlayerBodyCollection = {
    new PlayerBodyCollection(bodyParts.toList)
  }

}
