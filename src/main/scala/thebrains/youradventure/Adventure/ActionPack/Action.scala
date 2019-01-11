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
  @transient lazy val getConditions: List[Condition] = conditions

  def canBeDisplayed(p: Player): IO[Err, Boolean] = {
    conditions.foldLeft[IO[Err, Boolean]](IO.sync(true)) {
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

  def copy(
    name:        String = this.name,
    description: String = this.description,
    targetStep:  Either[StepName, Step] = this.targetStep,
    conditions:  List[Condition] = this.conditions
  ): Action = {
    new Action(
      name,
      description,
      targetStep,
      conditions
    )
  }
}

object Actions {
  def playerStatusMenu(
    player: Player,
    p:      Player => IO[Err, Step]
  ): IO[Err, Action] = {
    p(player).map { step =>
      Action("Player Status", "Look at your player status", step, Nil)
    }
  }

  final case object Exit
      extends Action(
        name = "Exit",
        description = "You are about to leave the game.",
        targetStep = Right(Steps.ExitStep),
        conditions = Nil
      )

  val BackActionName: StepName = "Back"

  final case class Back(step: Step)
      extends Action(BackActionName, "Go back to where you were?", Right(step))

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
