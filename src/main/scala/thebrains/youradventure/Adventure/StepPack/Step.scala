package thebrains.youradventure.Adventure.StepPack

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack._
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Adventure.StepPack.Step.StepName
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils.{Err, ErrorIO}

class Step(
  name:             StepName,
  description:      String,
  location:         Location,
  transformations:  TransformationCollection,
  availableActions: ActionCollection
) extends AssemblyItemTrait(name, description) {
  @transient lazy val getLocation: Location = location
  @transient lazy private val actionWithExit: ActionCollection = {
    if (availableActions.nonEmpty) {
      Actions.Exit ++ availableActions
    } else {
      ActionCollection.Empty
    }
  }

  private def playerMenu(player: Player): IO[Err, Step] = {
    this match {
      case s: PlayerStatusStep => s.getSourceStep.playerMenu(player)
      case s: Step             => Steps.playerStatusStep(s, player)
    }
  }

  def getActions(player: Player): IO[Err, ActionCollection] = {
    if (this.availableActions.isEmpty) {
      IO.sync(this.availableActions)
    } else {
      Actions
        .playerStatusMenu(player, playerMenu)
        .map { playerStep =>
          Actions.Exit ++ playerStep ++ availableActions
        }
    }
  }

  def getActions(playerMaybe: Maybe[Player]): IO[Err, ActionCollection] = {
    playerMaybe match {
      case Maybe.Just(p) => getActions(p)
      case Maybe.Empty() => IO.sync(actionWithExit)
    }
  }
}

final class PlayerStatusStep(
  step:   Step,
  player: Player
) extends Step(
      name = "Player Status",
      description = player.toStatus,
      location = Locations.Menu,
      transformations = TransformationCollection.Empty,
      availableActions = ActionCollection("Go back?")(Actions.Back(step))
    ) {
  @transient lazy val getSourceStep: Step = step
}

object Steps {
  def playerStatusStep(
    step:   Step,
    player: Player
  ): IO[Err, PlayerStatusStep] = {
    def getCorrectBackStep(step: Step): IO[Err, Step] = {
      step match {
        case p: PlayerStatusStep =>
          (for {
            actions <- p.getActions(player)
            action  <- actions.getAction(Actions.BackActionName)
          } yield {
            action.getTargetStep match {
              case Left(_)  => ErrorIO("Weird case", s"The back step was '${action.toString}'.")
              case Right(s) => getCorrectBackStep(s)
            }
          }).flatMap(identity)
        case s: Step => IO.sync(s)
      }
    }

    getCorrectBackStep(step).map(s => new PlayerStatusStep(s, player))
  }

  final case object EmptyStep
      extends Step(
        name = "Void",
        description = "This is a dead end",
        location = Locations.Void,
        transformations = TransformationCollection.Empty,
        availableActions = ActionCollection.Empty
      )

  final case object ExitStep
      extends Step(
        name = "Exit",
        description = "You are exiting the game.",
        location = Locations.Void,
        transformations = TransformationCollection.Empty,
        availableActions = ActionCollection.Empty
      )

}

object Step {
  type StepName = String

  def apply(
    name:             StepName,
    description:      String,
    location:         Location,
    transformations:  TransformationCollection,
    availableActions: ActionCollection
  ): Step = {
    new Step(name, description, location, transformations, availableActions)
  }
}
