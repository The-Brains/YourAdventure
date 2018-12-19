package thebrains.youradventure.Adventure.AttributePack

import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.Things

class Attribute(
  name:        String,
  description: String
) extends Things(name, description) {
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

  case object Intelligence
      extends Attribute(
        "intelligence",
        "TODO"
      )

}
