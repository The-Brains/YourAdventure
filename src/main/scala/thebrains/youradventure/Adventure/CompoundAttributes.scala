package thebrains.youradventure.Adventure

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.AttributePack.{Attribute, AttributeCollection, Attributes}

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
    attribute1: Maybe[AttributeType],
    attribute2: Maybe[AttributeType]
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

object CompoundAttributes {

  final case object Health
      extends CompoundAttributes2(
        name = "health",
        description = "The amount of health. If zero, you are dead.",
        attribute1 = Attributes.Strength,
        attribute2 = Attributes.Constitution
      ) {
    override def compose(
      attribute1: Maybe[AttributeType],
      attribute2: Maybe[AttributeType]
    ): AttributeType = {
      attribute1.getOrElse(0) + attribute2.getOrElse(0)
    }
  }

}
