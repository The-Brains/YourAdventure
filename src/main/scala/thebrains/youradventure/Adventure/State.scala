package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.StepPack.Step

case class State(
  currentStep:   Step,
  currentPlayer: Player
)
