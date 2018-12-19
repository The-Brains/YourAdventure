package thebrains.youradventure.Adventure

class Universe(
  availableRaces: List[Race],
  availableLocations: List[Location],
  startingStep: Step
) {
  def getStartingStep: Step = startingStep
}

object Universe {

  case object Void
    extends Universe(
      availableRaces = Nil,
      availableLocations = Nil,
      startingStep = Steps.EmptyStep
    )

  def apply(
    availableRaces: List[Race],
    availableLocations: List[Location],
    startingStep: Step
  ): Universe = {
    new Universe(availableRaces, availableLocations, startingStep)
  }
}
