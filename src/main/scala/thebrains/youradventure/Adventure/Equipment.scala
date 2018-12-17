package thebrains.youradventure.Adventure

case class Equipment(
  name:        String,
  description: String,
  bodyPart:    BodyPart,
  modifiers:   List[Transformation]
) extends Item(
      name,
      description,
      modifiers
    )
