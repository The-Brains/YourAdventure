package thebrains.youradventure.Adventure.BodyPack

import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Adventure.Equipment

class PlayerBodyCollection(bodyParts: List[PlayerBodyPart])
    extends AssemblyTrait[PlayerBodyCollection, PlayerBodyPart](bodyParts.toList) {
  override protected def wrap(items: PlayerBodyPart*): PlayerBodyCollection = {
    new PlayerBodyCollection(items.toList)
  }

  @transient lazy val equipments: List[Equipment] = bodyParts.flatMap(_.equipment.toOption)

  override protected def empty: PlayerBodyCollection = PlayerBodyCollection.Empty
}

object PlayerBodyCollection {

  case object Empty extends PlayerBodyCollection(Nil)

  def apply(bodyParts: PlayerBodyPart*): PlayerBodyCollection = {
    new PlayerBodyCollection(bodyParts.toList)
  }

}
