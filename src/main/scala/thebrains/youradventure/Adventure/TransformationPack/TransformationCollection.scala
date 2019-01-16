package thebrains.youradventure.Adventure.TransformationPack

import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute
import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Utils.Err
import thebrains.youradventure.Utils.ToIO._

class TransformationCollection(transformations: List[Transformation])
    extends AssemblyTrait[TransformationCollection, Transformation](transformations) {
  override protected def wrap(items: Transformation*): TransformationCollection = {
    new TransformationCollection(items.toList)
  }

  override protected def empty: TransformationCollection = TransformationCollection.Empty

  def safeApplyOn(p: PlayerAttribute): PlayerAttribute = {
    this.getItems.foldLeft(p) {
      case (attribute, t) =>
        t.safeAppliedTo(attribute)
    }
  }

  def applyOn(p: PlayerAttribute): IO[Err, PlayerAttribute] = {
    this.getItems.foldLeft(p.toIO) {
      case (attribute, t) =>
        attribute.flatMap(a => t.appliedTo(a))
    }
  }

  def revert(p: PlayerAttribute): IO[Err, PlayerAttribute] = {
    this.getItems.reverse.foldLeft(p.toIO) {
      case (attribute, t) =>
        attribute.flatMap(a => t.revert(a))
    }
  }

  def safeRevertOn(p: PlayerAttribute): PlayerAttribute = {
    this.getItems.reverse.foldLeft(p) {
      case (attribute, t) => t.safeRevert(attribute)
    }
  }
}

object TransformationCollection {

  final case object Empty extends TransformationCollection(Nil)

  def apply(transformations: Transformation*): TransformationCollection = {
    new TransformationCollection(transformations.toList)
  }
}
