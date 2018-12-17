package thebrains.youradventure.Adventure.Attribute

import thebrains.youradventure.Adventure._

case class PlayerAttribute(
  attribute: Attribute,
  value: PlayerAttribute.AttributeType
) {
  def ===(other: PlayerAttribute): Boolean = {
    this.attribute === other
  }

  def |+|(other: PlayerAttribute): Either[Error, PlayerAttribute] = {
    if (this === other) {
      Right(this.copy(value = this.value + other.value))
    } else {
      Left(
        Error(
          "Cannot combine attributes",
          s"Cannot combine attribute ${this.attribute.toString} with ${other.attribute.toString}"
        )
      )
    }
  }

  override def hashCode(): Int = attribute.getName.hashCode

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: PlayerAttribute => p === this
      case _ => false
    }
  }

  override def toString: String = s"${attribute.toString} -> $value"

  def asCollection: AttributeCollection = AttributeCollection(this)

  def ++(other: PlayerAttribute): AttributeCollection = asCollection ++ other

  def ++(other: AttributeCollection): AttributeCollection = other ++ this
}

object PlayerAttribute {
  type AttributeType = Int
  type AttributeTransformation = AttributeType => AttributeType
}
