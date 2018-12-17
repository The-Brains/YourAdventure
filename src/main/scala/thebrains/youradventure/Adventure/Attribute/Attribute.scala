package thebrains.youradventure.Adventure.Attribute

import PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.{CompoundAttributes2, Things}

class Attribute(
  name: String,
  description: String
) extends Things(name, description) {
  def getName: String = name

  def toPlayerAttribute(value: AttributeType): PlayerAttribute = {
    PlayerAttribute(
      attribute = this,
      value = value
    )
  }

  def ===(other: Attribute): Boolean = other.getName == this.getName

  def ===(other: PlayerAttribute): Boolean = other.attribute === this

}

object Attributes {

  case object Strength
    extends Attribute(
      name = "strength",
      description = "Contribute to health and melee damage"
    )

  case object Constitution
    extends Attribute(
      name = "constitution",
      description = "Contribute to health and armor"
    )

  case object Health
    extends CompoundAttributes2(
      name = "health",
      description = "The amount of health. If zero, you are dead.",
      attribute1 = Strength,
      attribute2 = Constitution
    ) {
    override def compose(
      attribute1: Option[AttributeType],
      attribute2: Option[AttributeType]
    ): AttributeType = {
      attribute1.getOrElse(0) + attribute2.getOrElse(0)
    }
  }

  case object Intelligence extends Attribute(
    "intelligence",
    "TODO"
  )

}
