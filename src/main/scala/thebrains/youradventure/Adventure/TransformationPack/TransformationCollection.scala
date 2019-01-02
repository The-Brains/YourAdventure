package thebrains.youradventure.Adventure.TransformationPack

import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait

class TransformationCollection(transformations: List[Transformation])
    extends AssemblyTrait[TransformationCollection, Transformation](transformations) {
  override protected def wrap(items: Transformation*): TransformationCollection = {
    new TransformationCollection(items.toList)
  }

  override protected def empty: TransformationCollection = TransformationCollection.Empty
}

object TransformationCollection {

  final case object Empty extends TransformationCollection(Nil)

  def apply(transformations: Transformation*): TransformationCollection = {
    new TransformationCollection(transformations.toList)
  }
}
