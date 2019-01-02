package thebrains.youradventure.Adventure.StepPack

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack._
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Adventure.StepPack.Step.StepName
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils.Error

class Step(
  name:             StepName,
  description:      String,
  location:         Location,
  transformations:  TransformationCollection,
  availableActions: ActionCollection
) extends AssemblyItemTrait(name, description) {
  @transient lazy val getLocation: Location = location

  private def playerMenu(player: Player): Step = {
    this match {
      case s: Steps.PlayerStatusStep => s.step.playerMenu(player)
      case s: Step                   => Steps.PlayerStatusStep(s, player)
    }
  }

  def getActions(player: Player): IO[Nothing, ActionCollection] = {
    IO.sync {
      if (this.availableActions.isEmpty) {
        this.availableActions
      } else {
        Actions.Exit ++ Actions.playerStatusMenu(player, playerMenu) ++ availableActions
      }
    }
  }

  def getActions(playerMaybe: Maybe[Player]): IO[Error, ActionCollection] = {
    playerMaybe match {
      case Maybe.Just(p) => getActions(p)
      case Maybe.Empty() => IO.sync(this.availableActions)
    }
  }
}

object Steps {

  case class PlayerStatusStep(
    step:   Step,
    player: Player
  ) extends Step(
        name = "Player Status",
        description = player.toStatus,
        location = Locations.Menu,
        transformations = TransformationCollection.Empty,
        availableActions = ActionCollection("Go back?")(
          Action("Back", "Go back to where you were?", Right(step))
        )
      )

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
