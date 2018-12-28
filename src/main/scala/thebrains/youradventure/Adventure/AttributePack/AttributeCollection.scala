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
          .map {
            case (_, attributeTransformations) =>
              val attributesToTransform = attributeTransformations.flatMap(_._2._1)
              val transformations = attributeTransformations.flatMap(_._2._2)

              val startedAttribute = AttributeCollection(attributesToTransform: _*).reduceAll

              transformations.foldLeft(startedAttribute) {
                case (io, t: Transformation) => io.flatMap(a => t >> a)
              }
          }
      )
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
}

object AttributeCollection {

  case object Empty extends AttributeCollection(Set.empty)

  def apply(a: PlayerAttribute*): AttributeCollection = {
    new AttributeCollection(a.toSet)
  }

  def apply(a: Set[PlayerAttribute]): AttributeCollection = new AttributeCollection(a)
}
