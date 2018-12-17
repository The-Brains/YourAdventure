package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Transformation.{Transformation, TransformationCollection}

class Item(
  name:        String,
  description: String,
  modifiers:   TransformationCollection
) extends Things(name, description)
