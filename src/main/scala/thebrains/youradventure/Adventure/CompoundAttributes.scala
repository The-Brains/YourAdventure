package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Attribute.PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.Attribute.{Attribute, AttributeCollection}

abstract class CompoundAttributes(
  name:        String,
  description: String
) extends Things(
      name,
      description
    ) {
  def getValue(attributes: AttributeCollection): PlayerValues
}

abstract class CompoundAttributes2(
  name:        String,
  description: String,
  attribute1:  Attribute,
  attribute2:  Attribute
) extends CompoundAttributes(
      name,
      description
    ) {

  def compose(
    attribute1: Option[AttributeType],
    attribute2: Option[AttributeType]
  ): AttributeType

  final override def getValue(attributes: AttributeCollection): PlayerValues = {
    val playerAttribute1 = attributes
      .find(attribute1 === _)
      .map(_.value)
    val playerAttribute2 = attributes
      .find(attribute2 === _)
      .map(_.value)

    PlayerValues(this, compose(playerAttribute1, playerAttribute2))
  }
}

abstract class CompoundAttributes3(
  name:        String,
  description: String,
  attribute1:  Attribute,
  attribute2:  Attribute,
  attribute3:  Attribute
) extends CompoundAttributes(
      name,
      description
    ) {
  def compose(
    attribute1: AttributeType,
    attribute2: AttributeType,
    attribute3: AttributeType
  ): AttributeType
}
