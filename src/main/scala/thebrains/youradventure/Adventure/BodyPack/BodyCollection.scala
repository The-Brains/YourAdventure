package thebrains.youradventure.Adventure.BodyPack

import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait

class BodyCollection(bodyParts: BodyPart*)
    extends AssemblyTrait[BodyCollection, BodyPart](bodyParts.toList) {
  override protected def wrap(items: BodyPart*): BodyCollection = {
    new BodyCollection(items: _*)
  }
}

object BodyCollection {

  case object Empty extends BodyCollection()

  def apply(bodyParts: BodyPart*): BodyCollection = new BodyCollection(bodyParts: _*)

}
