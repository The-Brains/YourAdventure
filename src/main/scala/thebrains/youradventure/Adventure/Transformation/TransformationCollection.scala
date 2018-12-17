package thebrains.youradventure.Adventure.Transformation

class TransformationCollection(transformations: List[Transformation])
  extends scalaz.Monoid[TransformationCollection] {

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

  override def zero: TransformationCollection = new TransformationCollection(Nil)

  override def append(
    f1: TransformationCollection,
    f2: => TransformationCollection
  ): TransformationCollection = {
    new TransformationCollection(f1.getTransformations ++ f2.getTransformations)
  }

  def ++(other: TransformationCollection): TransformationCollection = {
    append(this, other)
  }

  def ++(other: Transformation): TransformationCollection = {
    append(this, other.asCollection)
  }
}

object TransformationCollection {
  def apply(transformations: Transformation*): TransformationCollection = {
    new TransformationCollection(transformations.toList)
  }
}
