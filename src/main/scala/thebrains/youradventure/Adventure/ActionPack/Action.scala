package thebrains.youradventure.Adventure.ActionPack

import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils
import thebrains.youradventure.Utils.Error

case class Action(
  name: String,
  description: String,
  targetStep: Step
) extends AssemblyItemTrait(name, description) {
  def ++(availableActions: ActionCollection): IO[Error, ActionCollection] = {
    BastardActionCollection(this) ++ availableActions match {
      case a: ActionCollection => IO.sync(a)
      case _ => IO.fail(Error("Cannot convert",
        "Cannot convert to 'ActionCollection'."))
    }
  }

  override def |+|(
    other: AssemblyItemTrait
  ): IO[Utils.Error, AssemblyItemTrait] = {
    IO.fail(Error(
      "Cannot combine 'Action'.",
      "Cannot combine 'Action'."
    )
    )
  }

  def getStep: IO[Nothing, Step] = {
    IO.sync(targetStep)
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
