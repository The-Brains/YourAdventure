package thebrains.youradventure.Adventure.TransformationPack

import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait

class TransformationCollection(transformations: List[Transformation])
    extends AssemblyTrait[Transformation](transformations: _*) {
  override protected def wrap(items: Transformation*): AssemblyTrait[Transformation] = {
    new TransformationCollection(items.toList)
  }

  def ++(other: TransformationCollection): TransformationCollection = {
    super.++(other).asInstanceOf[TransformationCollection]
  }
}

object TransformationCollection {

  case object Empty extends TransformationCollection(Nil)

  def apply(transformations: Transformation*): TransformationCollection = {
    new TransformationCollection(transformations.toList)
  }
}
