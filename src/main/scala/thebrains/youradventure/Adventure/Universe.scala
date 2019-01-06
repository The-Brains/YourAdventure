package thebrains.youradventure.Adventure

import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.ListImplicits._
import thebrains.youradventure.Adventure.StepPack._
import thebrains.youradventure.Utils.{Err, ErrorIO}

class Universe(
  availableRaces: List[Race],
  availableSteps: StepCollection,
  startingStep:   Step
) {
  @transient lazy val getAvailableRaces: List[Race] = availableRaces

  //  @transient lazy val availableLocations: Set[Location] =
  //    exploreAllSteps(startingStep, Nil)(_.getLocation).toSet
  //
  //  private def exploreAllSteps[A](currentStep: Step, acc: List[A] = Nil)(f: Step => A):
  //  List[A] = {
  //    val t = (for {
  //      allActions: ActionCollection <- currentStep.getActions(Maybe.empty[Player])
  //      action: Action <- allActions.getActions
  //      step: Step <- action.getStep
  //      item: A <- exploreAllSteps(step, acc)(f)
  //    } yield {
  //      item
  //    })
  //
  //    t
  //  }

  @transient lazy val getStartingStep: Step = startingStep

  @transient lazy val getAvailableSteps: StepCollection = availableSteps
}

object Universe {

  final case object Void
      extends Universe(
        availableRaces = Nil,
        availableSteps = StepCollection.Empty,
        startingStep = Steps.EmptyStep
      )

  def apply(
    availableRaces: List[Race],
    availableSteps: StepCollection,
    startingStep:   Step
  ): IO[Err, Universe] = {
    if (availableSteps.outMap(_.getName).isUnique) {
      IO.sync(new Universe(availableRaces, availableSteps, startingStep))
    } else {
      ErrorIO(
        "Step list is not unique",
        s"The list of steps have duplicate names: " +
          s"${availableSteps.getExtras.map(_.getName).mkString(", ")}"
      )
    }
  }
}
