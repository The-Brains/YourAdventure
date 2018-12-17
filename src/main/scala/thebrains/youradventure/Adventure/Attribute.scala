package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.PlayerAttribute.AttributeType

case class Attribute(
  name: String,
  description: String
) extends Things(name, description) {
  def toPlayerAttribute(value: AttributeType): PlayerAttribute = {
    PlayerAttribute(
      attribute = this,
      value = value
    )
  }

  def ===(other: Attribute): Boolean = other.name == this.name

  def ===(other: PlayerAttribute): Boolean = other.attribute === this

}

object Attributes {
  val Strength: Attribute = Attribute(
    "force",
    "Contribute to health and melee damage"
  )

  val Constitution: Attribute = Attribute(
    "consitution",
    "Contribute to health and armor"
  )

  val Health: CompoundAttributes = new CompoundAttributes2(
    "health",
    "The amount of health. If zero, you are dead.",
    Strength,
    Constitution
  ) {
    override def compose(
      attribute1: Option[AttributeType],
      attribute2: Option[AttributeType]
    ): AttributeType = {
      attribute1.getOrElse(0) + attribute2.getOrElse(0)
    }
  }
}
