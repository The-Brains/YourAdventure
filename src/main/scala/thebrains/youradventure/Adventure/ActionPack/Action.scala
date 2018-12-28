package thebrains.youradventure.Adventure.ActionPack

import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Adventure.ConditionPack.Condition
import thebrains.youradventure.Adventure.StepPack.Step.StepName
import thebrains.youradventure.Adventure.StepPack.{Step, StepCollection}
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils
import thebrains.youradventure.Utils.Error

class Action(
  name: String,
  description: String,
  targetStep: Either[StepName, Step],
  conditions: List[Condition] = Nil
) extends AssemblyItemTrait(name, description) {
  def ++(availableActions: ActionCollection): IO[Error, ActionCollection] = {
    IO.sync(BastardActionCollection(this) ++ availableActions)
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
      case Right(step) => IO.sync(step)
      case Left(stepName) => availableSteps.getStep(stepName)
    }
  }
}

object Actions {
  def playerStatusMenu(
    player: Player,
    p: Player => Step
  ): Action = {
    new Action("Player Status", "Look at your player status", Right(p(player)))
  }
}

object Action {
  def apply(
    name: String,
    description: String,
    targetStep: Either[StepName, Step],
    conditions: List[Condition] = Nil
  ): Action = {
    new Action(name, description, targetStep, conditions)
  }

  def apply(
    name: String,
    description: String,
    targetStep: StepName,
    conditions: List[Condition]
  ): Action = {
    new Action(name, description, Left(targetStep), conditions)
  }

  def apply(
    name: String,
    description: String,
    targetStep: Step,
    conditions: List[Condition]
  ): Action = {
    new Action(name, description, Right(targetStep), conditions)
  }
}
