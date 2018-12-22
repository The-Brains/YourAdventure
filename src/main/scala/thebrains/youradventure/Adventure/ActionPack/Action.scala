package thebrains.youradventure.Adventure.ActionPack

import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils
import thebrains.youradventure.Utils.Error

case class Action(
  name: String,
  description: String,
  targetStep: Step
) extends AssemblyItemTrait(name, description) {
  def ++(availableActions: ActionCollection): Either[Error, ActionCollection] = {
    BastardActionCollection(this) ++ availableActions match {
      case a: ActionCollection => Right(a)
      case _ => Left(Error("Cannot convert",
        "Cannot convert to 'ActionCollection'."))
    }
  }

  override def |+|(
    other: AssemblyItemTrait
  ): Either[Utils.Error, AssemblyItemTrait] = {
    Left(Error(
      "Cannot combine 'Action'.",
      "Cannot combine 'Action'."
    )
    )
  }
}

object Actions {
  def playerStatusMenu(
    player: Player,
    p: Player => Step
  ): Action = {
    Action("Player Status", "Look at your player status", p(player))
  }
}
