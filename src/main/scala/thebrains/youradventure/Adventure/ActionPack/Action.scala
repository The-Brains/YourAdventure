package thebrains.youradventure.Adventure.ActionPack

import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Adventure.StepPack.Step.StepName
import thebrains.youradventure.Adventure.StepPack.{Step, StepCollection}
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils
import thebrains.youradventure.Utils.Error

case class Action(
  name:        String,
  description: String,
  targetStep:  Either[StepName, Step]
) extends AssemblyItemTrait(name, description) {
  def ++(availableActions: ActionCollection): IO[Error, ActionCollection] = {
    BastardActionCollection(this) ++ availableActions match {
      case a: ActionCollection => IO.sync(a)
      case _ => IO.fail(Error("Cannot convert", "Cannot convert to 'ActionCollection'."))
    }
  }

  override def |+|(other: AssemblyItemTrait): IO[Utils.Error, AssemblyItemTrait] = {
    IO.fail(
      Error(
        "Cannot combine 'Action'.",
        "Cannot combine 'Action'."
      )
    )
  }

  def getStep(availableSteps: StepCollection): IO[Error, Step] = {
    targetStep match {
      case Right(step)    => IO.sync(step)
      case Left(stepName) => availableSteps.getStep(stepName)
    }
  }
}

object Actions {
  def playerStatusMenu(
    player: Player,
    p:      Player => Step
  ): Action = {
    Action("Player Status", "Look at your player status", Right(p(player)))
  }
}
