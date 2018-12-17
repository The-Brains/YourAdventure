package thebrains.youradventure.Adventure

case class Location(
  name:        String,
  description: String
) extends Things(name, description)
