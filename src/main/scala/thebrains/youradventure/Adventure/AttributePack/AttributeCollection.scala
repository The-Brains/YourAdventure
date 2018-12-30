package thebrains.youradventure.Adventure.AttributePack

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.Utils.Error

class AttributeCollection(attributes: Set[PlayerAttribute])
  extends AssemblyTrait[AttributeCollection, PlayerAttribute](attributes.toList) {

  def <<(applyTransformations: TransformationCollection): IO[Error, AttributeCollection] = {
    IO.sequence(
      (this.toCustomMap.mapValues(p => (Some(p), None)).toList ++
        applyTransformations.toCustomMap.mapValues(t => (None, Some(t))).toList)
        .groupBy(_._1)
        .map { case (_, attributeTransformations) =>
          (
            attributeTransformations.flatMap(_._2._1),
            attributeTransformations.flatMap(_._2._2)
          )
        }
        .map {
          case (Nil, _) => IO.sync(Maybe.empty)
          case (attributesToTransform, Nil) =>
            AttributeCollection(attributesToTransform: _*).reduceAll.map(Maybe.just)
          case (attributesToTransform, transformations) =>
            val startedAttribute = AttributeCollection(attributesToTransform: _*).reduceAll
            transformations
              .foldLeft(startedAttribute) {
                case (io, t: Transformation) => io.flatMap(a => t appliedTo a)
              }
              .map(Maybe.just)
        }
    )
      .map(_.flatMap(_.toOption))
      .map(s => AttributeCollection(s.toSet))
  }

  def getAttribute[A <: Attribute](attribute: A): Maybe[PlayerAttribute] = {
    this.find(attribute === _)
  }

  def getAttributeValue[A <: Attribute](attribute: A): Maybe[AttributeType] = {
    this.getAttribute(attribute).map(_.value)
  }

  override protected def wrap(items: PlayerAttribute*): AttributeCollection = {
    new AttributeCollection(items.toSet)
  }

  override protected def empty: AttributeCollection = AttributeCollection.Empty
}

object AttributeCollection {

  case object Empty extends AttributeCollection(Set.empty)

  def apply(a: PlayerAttribute*): AttributeCollection = {
    new AttributeCollection(a.toSet)
  }

  def apply(a: Set[PlayerAttribute]): AttributeCollection = new AttributeCollection(a)
}
