package thebrains.youradventure.Adventure

class Universe(
  availableRaces:     List[Race],
  availableLocations: List[Location],
  startingStep:       Step
)

object Universe {

  case object Void
      extends Universe(
        availableRaces = Nil,
        availableLocations = Nil,
        startingStep = Step.EmptyStep
      )

}
