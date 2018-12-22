package thebrains.youradventure.Adventure

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack._
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.Utils.Error

class Step(
  name: String,
  description: String,
  location: Location,
  transformations: TransformationCollection,
  availableActions: ActionCollection
) extends Things(name, description) {
  private def playerMenu(player: Player): Step = {
    this match {
      case s: Steps.PlayerStatusStep => s.step.playerMenu(player)
      case s: Step => Steps.PlayerStatusStep(s, player)
    }
  }

  def getActions(playerMaybe: Maybe[Player]): Either[Error, ActionCollection] = {
    if (this.availableActions.isEmpty) {
      Right(this.availableActions)
    } else {
      playerMaybe
        .map(player => Actions.playerStatusMenu(player, playerMenu)) match {
        case Maybe.Just(playerMenu) => playerMenu ++ availableActions
        case Maybe.Empty() => Right(availableActions)
      }
    }
  }
}

object Steps {

  case class PlayerStatusStep(
    step: Step,
    player: Player
  ) extends Step(
    name = "Player Status",
    description = player.toStatus,
    location = Locations.Menu,
    transformations = TransformationCollection.Empty,
    availableActions = ActionCollection("Go back?")(
      Action("Back", "Go back to where you were?", step)
    )
  )

  case object EmptyStep
    extends Step(
      name = "Void",
      description = "This is a dead end",
      location = Locations.Void,
      transformations = TransformationCollection.Empty,
      availableActions = ActionCollection.Empty
    )

}

object Step {
  def apply(
    name: String,
    description: String,
    location: Location,
    transformations: TransformationCollection,
    availableActions: ActionCollection
  ): Step = {
    new Step(name, description, location, transformations, availableActions)
  }
}
