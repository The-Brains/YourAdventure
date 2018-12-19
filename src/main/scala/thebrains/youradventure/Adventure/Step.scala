package thebrains.youradventure.Adventure

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack._
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection

class Step(
  name: String,
  description: String,
  location: Location,
  transformations: TransformationCollection,
  availableActions: ActionCollection
) extends Things(name, description) {
  private def playerMenu(player: Player): Step = {
    this match {
      case s: Step.PlayerStatusStep => s.step.playerMenu(player)
      case s: Step => Step.PlayerStatusStep(s, player)
    }
  }

  def getActions(playerMaybe: Maybe[Player]): ActionCollection = {
    playerMaybe
      .map(player => Actions.playerStatusMenu(player, playerMenu)) match {
      case Maybe.Just(playerMenu) => playerMenu ++ availableActions
      case Maybe.Empty() => availableActions
    }

  }
}

object Step {

  case class PlayerStatusStep(
    step: Step,
    player: Player
  ) extends Step(
    name = "Player Status",
    description = player.toStatus,
    location = Location.Menu,
    transformations = TransformationCollection.Empty,
    availableActions = ActionCollection("Go back?")(
      Action("Back", "Go back to where you were?", step)
    )
  )

  case object EmptyStep
    extends Step(
      name = "Void",
      description = "This is a dead end",
      location = Location.Void,
      transformations = TransformationCollection.Empty,
      availableActions = ActionCollection.Empty
    )

}
