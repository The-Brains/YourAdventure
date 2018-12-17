package thebrains.youradventure.Adventure

class Consumable(
  name:        String,
  description: String,
  modifiers:   List[Transformation]
) extends Item(
      name,
      description,
      modifiers
    ) {}
