package thebrains.youradventure.Adventure

case class Action(
  name:        String,
  description: String,
  targetStep:  Step
) extends Things(name, description)
