package thebrains.youradventure.Adventure.BodyPack

import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait

class BodyCollection(bodyPart: List[BodyPart])
    extends AssemblyTrait[BodyCollection, BodyPart](bodyPart) {
  override protected def wrap(items: BodyPart*): BodyCollection = {
    new BodyCollection(items.toList)
  }

  @transient lazy val toPlayerBodyCollection: PlayerBodyCollection =
    PlayerBodyCollection(this.outMap(_.toPlayerBodyPart): _*)

  override protected def empty: BodyCollection = BodyCollection.Empty
}

object BodyCollection {

  case object Empty extends BodyCollection(Nil)

  def apply(bodyParts: BodyPart*): BodyCollection = {
    new BodyCollection(bodyParts.toList)
  }

}
