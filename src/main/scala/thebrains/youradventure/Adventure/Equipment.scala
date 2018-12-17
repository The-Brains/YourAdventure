package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Transformation.TransformationCollection

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
