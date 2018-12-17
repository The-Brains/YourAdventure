package thebrains.youradventure.Adventure

case class Character(
  name:        String,
  description: String
) extends Things(name, description)
