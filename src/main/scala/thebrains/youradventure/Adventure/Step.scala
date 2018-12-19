package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.ActionPack.ActionCollection
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection

class Step(
  name:             String,
  description:      String,
  location:         Location,
  transformations:  TransformationCollection,
  availableActions: ActionCollection
) extends Things(name, description) {
  def getActions: ActionCollection = availableActions
}

object Step {

  case object EmptyStep
      extends Step(
        name = "Void",
        description = "This is a dead end",
        location = Location.Void,
        transformations = TransformationCollection.Empty,
        availableActions = ActionCollection.Empty
      )

}
