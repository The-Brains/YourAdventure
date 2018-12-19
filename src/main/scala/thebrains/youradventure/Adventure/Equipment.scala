package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.BodyPack.BodyPart
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection

case class Equipment(
  name:        String,
  description: String,
  bodyPart:    BodyPart,
  modifiers:   TransformationCollection
) extends Item(
      name,
      description,
      modifiers
    )
