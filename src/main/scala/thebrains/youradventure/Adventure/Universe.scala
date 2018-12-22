package thebrains.youradventure.Adventure

class Universe(
  availableRaces: List[Race],
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

  def getStartingStep: Step = startingStep
}

object Universe {

  case object Void
      extends Universe(
        availableRaces = Nil,
        startingStep = Steps.EmptyStep
      )

  def apply(
    availableRaces: List[Race],
    startingStep:   Step
  ): Universe = {
    new Universe(availableRaces, startingStep)
  }
}
