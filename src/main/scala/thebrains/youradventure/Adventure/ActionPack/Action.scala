package thebrains.youradventure.Adventure.ActionPack

import thebrains.youradventure.Adventure.{Step, Things}

case class Action(
  name:        String,
  description: String,
  targetStep:  Step
) extends Things(name, description) {
  def asCollection: BastardActionCollection = BastardActionCollection(this)
}
