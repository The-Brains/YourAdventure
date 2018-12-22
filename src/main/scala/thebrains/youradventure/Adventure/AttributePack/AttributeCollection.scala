package thebrains.youradventure.Adventure.AttributePack

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.Utils.Error

class AttributeCollection(attributes: Set[PlayerAttribute])
  extends AssemblyTrait[PlayerAttribute](attributes.toSeq: _*) {

  def <<(applyTransformations: TransformationCollection): AttributeCollection = {
    val newAttributes = (this.toCustomMap.mapValues(p => (Some(p), None)).toList ++
      applyTransformations.toCustomMap.mapValues(t => (None, Some(t))).toList)
      .groupBy(_._1)
      .map {
        case (_, attributeTransformations) =>
          val transformations = attributeTransformations.flatMap(_._2._2)
          val attributesToTransform = attributeTransformations.flatMap(_._2._1)

          val startedAttribute = AttributeCollection(attributesToTransform: _*).reduceAll
          transformations.foldLeft[Either[Error, PlayerAttribute]](startedAttribute) {
            case (Right(attribute), transformation: Transformation) => transformation >> attribute
            case (error, _) => error
          }
      }
      .filter(_.isRight)
      .map(_.right.get)
      .toSet

    AttributeCollection(newAttributes)
  }

  def getAttribute[A <: Attribute](attribute: A): Maybe[PlayerAttribute] = {
    this.find(attribute === _)
  }

  def getAttributeValue[A <: Attribute](attribute: A): Maybe[AttributeType] = {
    this.getAttribute(attribute).map(_.value)
  }

  override protected def wrap(
    items: PlayerAttribute*
  ): AssemblyTrait[PlayerAttribute] = {
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
