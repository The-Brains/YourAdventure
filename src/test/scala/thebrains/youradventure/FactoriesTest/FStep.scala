package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack.ActionCollection
import thebrains.youradventure.Adventure.Location
import thebrains.youradventure.Adventure.StepPack.Step
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random

object FStep {
  private val DefaultNameLength:        Int = 5
  private val DefaultDescriptionLength: Int = 144

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
