package thebrains.youradventure.Game

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.Action
import thebrains.youradventure.Adventure.StepPack.Step
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FPTerminalIO.Renderer
import thebrains.youradventure.Utils.Err
import thebrains.youradventure.Utils.ToOption._

private[Game] class Updater(game: GameStatus) {
  private def copy(
    universe:      Maybe[Universe] = Maybe.empty,
    currentStep:   Maybe[Step] = Maybe.empty,
    currentAction: Maybe[Action] = Maybe.empty,
    player:        Maybe[PlayerTrait] = Maybe.empty,
    renderer:      Maybe[Renderer] = Maybe.empty,
    currentError:  Maybe[Err] = Maybe.empty
  ): GameStatus = {
    new GameStatus(
      universe = universe.getOrElse(this.game.getUniverse),
      currentStep = currentStep.orElse(this.game.getCurrentStep),
      currentAction = currentAction.orElse(this.game.getCurrentAction),
      player = player.orElse(this.game.getPlayer),
      renderer = renderer.getOrElse(this.game.getRenderer),
      currentError = currentError.orElse(this.game.getCurrentError)
    )
  }

  def withError(error: Err): GameStatus = {
    this.copy(currentError = error.just)
  }

  def withPlayer(p: PlayerTrait): IO[Err, GameStatus] = {
    IO.sync(this.copy(player = p.just))
  }

  def withStep(step: Step): IO[Err, GameStatus] = {
    IO.sync(this.copy(currentStep = step.just))
  }

  def withAction(a: Action): IO[Err, GameStatus] = {
    IO.sync(this.copy(currentAction = a.just))
  }

  def removeAction(): IO[Err, GameStatus] = {
    IO.sync(
      new GameStatus(
        universe = this.game.getUniverse,
        currentStep = this.game.getCurrentStep,
        currentAction = Maybe.empty,
        player = this.game.getPlayer,
        renderer = this.game.getRenderer,
        currentError = this.game.getCurrentError
      )
    )
  }

  def removeStep(): IO[Err, GameStatus] = {
    IO.sync(
      new GameStatus(
        universe = this.game.getUniverse,
        currentStep = Maybe.empty,
        currentAction = Maybe.empty,
        player = this.game.getPlayer,
        renderer = this.game.getRenderer,
        currentError = Maybe.empty
      )
    )
  }

  def removeError(): GameStatus = {
    new GameStatus(
      universe = this.game.getUniverse,
      currentStep = this.game.getCurrentStep,
      currentAction = this.game.getCurrentAction,
      player = this.game.getPlayer,
      renderer = this.game.getRenderer,
      currentError = Maybe.empty
    )
  }
}
