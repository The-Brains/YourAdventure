package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Attribute.PlayerAttribute.AttributeType

case class PlayerValues(
  attribute: CompoundAttributes,
  value:     AttributeType
)
