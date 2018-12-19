package thebrains.youradventure.Adventure.BodyPack

class BodyCollection {}

object BodyCollection extends scalaz.Monoid[BodyCollection] {

  case object Empty extends BodyCollection()

  override def zero: BodyCollection = Empty

  override def append(
    f1: BodyCollection,
    f2: => BodyCollection
  ): BodyCollection = {
    ???
  }
}
