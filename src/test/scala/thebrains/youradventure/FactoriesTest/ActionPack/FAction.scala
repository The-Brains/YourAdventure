package thebrains.youradventure.FactoriesTest.ActionPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack.Action
import thebrains.youradventure.Adventure.ConditionPack.Condition
import thebrains.youradventure.Adventure.StepPack.Step
import thebrains.youradventure.Adventure.StepPack.Step.StepName
import thebrains.youradventure.FactoriesTest.StepPack.FStep
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random
import thebrains.youradventure.FactoriesTest.DefaultValues._

object FAction {
  def apply(
    name:        Maybe[String] = Maybe.empty,
    description: Maybe[String] = Maybe.empty,
    targetStep:  Maybe[Either[StepName, Step]] = Maybe.empty,
    conditions:  List[Condition] = Nil
  )(
    implicit r: Random
  ): Action = {
    new Action(
      name = name getOrElse RandomMachine.getString(DefaultNameLength),
      description = description getOrElse RandomMachine.getString(DefaultDescriptionLength),
      targetStep = targetStep getOrElse RandomMachine.getEither[StepName, Step](
        RandomMachine.getString(DefaultTargetStepNameLength),
        FStep()
      ),
      conditions = conditions
    )
  }
}
