package thebrains.youradventure.Adventure

class Item(
  name:        String,
  description: String,
  modifiers:   List[Transformation]
) extends Things(name, description)
