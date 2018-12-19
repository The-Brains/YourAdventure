package thebrains.youradventure.Adventure.ActionPack

import thebrains.youradventure.Adventure._

case class Action(
  name: String,
  description: String,
  targetStep: Step
) extends Things(name, description) {
  def asCollection: BastardActionCollection = BastardActionCollection(this)

  def ++(other: Action): BastardActionCollection = asCollection ++ other

  def ++(other: BastardActionCollection): BastardActionCollection = other ++ this

  def ++(other: ActionCollection): ActionCollection = asCollection ++ other

}

object Actions {
  def playerStatusMenu(player: Player, p: Player => Step): Action = {
    Action("Player Status", "Look at your player status", p(player))
  }
}
