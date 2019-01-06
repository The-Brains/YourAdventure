package thebrains.youradventure.Adventure.ActionPack

import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Adventure.ConditionPack.Condition
import thebrains.youradventure.Adventure.StepPack.Step.StepName
import thebrains.youradventure.Adventure.StepPack._
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils.{Err, ErrorIO}

class Action(
  name:        String,
  description: String,
  targetStep:  Either[StepName, Step],
  conditions:  List[Condition] = Nil
) extends AssemblyItemTrait(name, description) {
  @transient lazy val getTargetStep: Either[StepName, Step] = targetStep

  def canBeDisplayed(p: Player): IO[Err, Boolean] = {
    conditions.foldLeft(IO.fromEither[Err, Boolean](Right(true))) {
      case (acc, condition) =>
        for {
          a <- acc
          c <- condition.isTrueFor(p)
        } yield {
          a && c
        }
    }
  }

  def ++(availableActions: ActionCollection): ActionCollection = {
    BastardActionCollection(this) ++ availableActions
  }

  def ++(availableActions: BastardActionCollection): BastardActionCollection = {
    BastardActionCollection(this) ++ availableActions
  }

  def ++(availableActions: Action): BastardActionCollection = {
    BastardActionCollection(this) ++ BastardActionCollection(availableActions)
  }

  override def |+|(other: AssemblyItemTrait): IO[Err, AssemblyItemTrait] = {
    ErrorIO(
      "Cannot combine 'Action'.",
      "Cannot combine 'Action'."
    )
  }

  def getStep(availableSteps: StepCollection): IO[Err, Step] = {
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
    Action("Player Status", "Look at your player status", p(player), Nil)
  }

  final case object Exit
      extends Action("Exit", "You are about to leave the game.", Right(Steps.ExitStep), Nil)

}

object Action {
  def apply(
    name:        String,
    description: String,
    targetStep:  Either[StepName, Step],
    conditions:  List[Condition] = Nil
  ): Action = {
    new Action(name, description, targetStep, conditions)
  }

  def apply(
    name:        String,
    description: String,
    targetStep:  StepName,
    conditions:  List[Condition]
  ): Action = {
    new Action(name, description, Left(targetStep), conditions)
  }

  def apply(
    name:        String,
    description: String,
    targetStep:  Step,
    conditions:  List[Condition]
  ): Action = {
    new Action(name, description, Right(targetStep), conditions)
  }
}
