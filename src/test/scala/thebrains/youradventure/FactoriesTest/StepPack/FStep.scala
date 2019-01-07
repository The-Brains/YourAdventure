package thebrains.youradventure.FactoriesTest.StepPack

import thebrains.youradventure.FactoriesTest.DefaultValues._
import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack.ActionCollection
import thebrains.youradventure.Adventure.Location
import thebrains.youradventure.Adventure.StepPack.Step
import thebrains.youradventure.Adventure.StepPack.Step.StepName
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.FactoriesTest.ActionPack.{FAction, FActionCollection}
import thebrains.youradventure.FactoriesTest.FLocation
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine
import thebrains.youradventure.Utils.ToOption._

import scala.util.Random

object FStep {
  def apply(
    name:             Maybe[String] = Maybe.empty,
    description:      Maybe[String] = Maybe.empty,
    location:         Maybe[Location] = Maybe.empty,
    transformations:  TransformationCollection = TransformationCollection.Empty,
    availableActions: ActionCollection = ActionCollection.Empty
  )(
    implicit r: Random
  ): Step = {
    new Step(
      name = name getOrElse RandomMachine.getString(DefaultNameLength),
      description = description getOrElse RandomMachine.getString(DefaultDescriptionLength),
      location = location getOrElse FLocation(),
      transformations = transformations,
      availableActions = availableActions
    )
  }
}

object FStepToStep {
  def apply(
    name:            Maybe[String] = Maybe.empty,
    description:     Maybe[String] = Maybe.empty,
    location:        Maybe[Location] = Maybe.empty,
    transformations: TransformationCollection = TransformationCollection.Empty,
    targetSteps:     List[Either[StepName, Step]] = Nil
  )(
    implicit r: Random
  ): Step = {
    new Step(
      name = name getOrElse RandomMachine.getString(DefaultNameLength),
      description = description getOrElse RandomMachine.getString(DefaultDescriptionLength),
      location = location getOrElse FLocation(),
      transformations = transformations,
      availableActions = FActionCollection(
        actions = targetSteps.map(s => FAction(targetStep = s.just)).just
      )
    )
  }
}
