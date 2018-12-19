package thebrains.youradventure.Adventure.TransformationPack

class TransformationCollection(transformations: List[Transformation]) {

  def getTransformations: List[Transformation] = this.transformations

  def toCustomMap: Map[String, Transformation] = {
    transformations.map(t => (t.attribute.getName, t)).toMap
  }

  def map(f: Transformation => Transformation): List[Transformation] = {
    transformations.map(f)
  }

  def flatMap(f: Transformation => List[Transformation]): List[Transformation] = {
    transformations.flatMap(f)
  }

  def filter(p: Transformation ⇒ Boolean): List[Transformation] = {
    transformations.filter(p)
  }

  def foreach(f: Transformation => Unit): Unit = {
    transformations.foreach(f)
  }

  def ++(other: TransformationCollection): TransformationCollection = {
    TransformationCollection.append(this, other)
  }

  def ++(other: Transformation): TransformationCollection = {
    TransformationCollection.append(this, other.asCollection)
  }
}

object TransformationCollection extends scalaz.Monoid[TransformationCollection] {

  case object Empty extends TransformationCollection(Nil)

  def apply(transformations: Transformation*): TransformationCollection = {
    new TransformationCollection(transformations.toList)
  }

  override def zero: TransformationCollection = TransformationCollection.Empty

  override def append(
    f1: TransformationCollection,
    f2: => TransformationCollection
  ): TransformationCollection = {
    new TransformationCollection(f1.getTransformations ++ f2.getTransformations)
  }
}
