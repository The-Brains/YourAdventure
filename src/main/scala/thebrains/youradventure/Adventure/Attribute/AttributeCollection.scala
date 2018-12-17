package thebrains.youradventure.Adventure.Attribute

import thebrains.youradventure.Adventure.Attribute.PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.Error
import thebrains.youradventure.Adventure.Transformation.{Transformation, TransformationCollection}

class AttributeCollection(attributes: Set[PlayerAttribute])
  extends scalaz.Monoid[AttributeCollection] {
  private def toCustomMap: Map[String, PlayerAttribute] = {
    attributes.map(a => (a.attribute.getName, a)).toMap
  }

  private def reduceAll: Either[Error, PlayerAttribute] = {
    attributes
      .foldLeft[Either[Error, PlayerAttribute]](Left(Error.Empty)) {
      case (Right(a), b) => a |+| b
      case (Left(_), b) => Right(b)
    }
  }

  def <<(applyTransformations: TransformationCollection): AttributeCollection = {
    val newAttributes = (this.toCustomMap.mapValues(p => (Some(p), None)).toList ++
      applyTransformations.toCustomMap.mapValues(t => (None, Some(t))).toList)
      .groupBy(_._1)
      .map {
        case (_, attributeTransformations) =>
          val transformations = attributeTransformations.flatMap(_._2._2)
          val attributesToTransform = attributeTransformations.flatMap(_._2._1).toSeq

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

  def map(f: PlayerAttribute => PlayerAttribute): Set[PlayerAttribute] = {
    attributes.map(f)
  }

  def flatMap(f: PlayerAttribute => Set[PlayerAttribute]): Set[PlayerAttribute] = {
    attributes.flatMap(f)
  }

  def filter(p: PlayerAttribute ⇒ Boolean): Set[PlayerAttribute] = {
    attributes.filter(p)
  }

  def foreach(f: PlayerAttribute => Unit): Unit = {
    attributes.foreach(f)
  }

  def find(p: PlayerAttribute => Boolean): Option[PlayerAttribute] = {
    attributes.find(p)
  }

  def getAttribute(attribute: Attribute): Option[PlayerAttribute] = {
    this.find(attribute === _)
  }

  def getAttributeValue(attribute: Attribute): Option[AttributeType] = {
    this.getAttribute(attribute).map(_.value)
  }

  override def zero: AttributeCollection = {
    new AttributeCollection(Set.empty)
  }

  override def append(
    f1: AttributeCollection,
    f2: => AttributeCollection
  ): AttributeCollection = {
    new AttributeCollection(
      (f1.toCustomMap ++ f2.toCustomMap)
        .groupBy(_._1)
        .map {
          case (_, groupedAttributes) =>
            AttributeCollection(groupedAttributes.values.toSeq: _*).reduceAll
        }
        .filter(_.isRight)
        .map(_.right.get)
        .toSet
    )
  }

  def ++(other: AttributeCollection): AttributeCollection = {
    append(this, other)
  }

  def ++(other: PlayerAttribute): AttributeCollection = {
    append(this, other.asCollection)
  }
}

object AttributeCollection {
  def apply(a: PlayerAttribute*): AttributeCollection = {
    new AttributeCollection(a.toSet)
  }

  def apply(a: Set[PlayerAttribute]): AttributeCollection = new AttributeCollection(a)
}
