package thebrains.youradventure.Adventure.BodyPack

import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Utils
import thebrains.youradventure.Utils.Error

class BodyCollection(bodyParts: BodyPart*) extends AssemblyTrait[BodyPart](bodyParts: _*) {
  override protected def wrap(
    items: BodyPart*
  ): BodyCollection = {
    new BodyCollection(items: _*)
  }
}

object BodyCollection {

  case object Empty extends BodyCollection()

  def apply(bodyParts: BodyPart*): BodyCollection = new BodyCollection(bodyParts: _*)

}
