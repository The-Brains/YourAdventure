package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Transformation.TransformationCollection

class Consumable(
  name:        String,
  description: String,
  modifiers:   TransformationCollection
) extends Item(
      name,
      description,
      modifiers
    ) {}
