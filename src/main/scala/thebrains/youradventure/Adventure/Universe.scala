package thebrains.youradventure.Adventure

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.CollectionPack.ListImplicits._
import thebrains.youradventure.Adventure.StepPack._
import thebrains.youradventure.Utils.{Err, ErrorIO}
import thebrains.youradventure.Adventure.CollectionPack.ToSortedSet._
import scala.collection.immutable.SortedSet

class Universe(
  availableRaces: List[Race],
  availableSteps: StepCollection,
  startingStep:   Step
) {
  @transient lazy val getAvailableRaces: List[Race] = availableRaces

  @transient lazy val getAllSteps: IO[Err, SortedSet[Step]] =
    exploreAllSteps(startingStep)(identity).map(_.toSortedSet)

  @transient lazy val availableLocations: IO[Err, SortedSet[Location]] =
    exploreAllSteps(startingStep)(_.getLocation).map(_.toSortedSet)

  private def exploreAllSteps[A](currentStep: Step)(f: Step => A): IO[Err, List[A]] = {
    currentStep
      .getActions(Maybe.empty[Player])
      .flatMap { allActions =>
        IO.sequence {
            allActions.getActions
              .map { action =>
                action
                  .getStep(getAvailableSteps)
                  .flatMap { step =>
                    exploreAllSteps(step)(f)
                  }
              }
          }
          .map(_.flatten)
      }
      .map { next =>
        List(f(currentStep)) ++ next
      }
  }

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
