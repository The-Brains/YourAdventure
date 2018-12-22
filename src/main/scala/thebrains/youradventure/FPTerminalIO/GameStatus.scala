package thebrains.youradventure.FPTerminalIO

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.Action
import thebrains.youradventure.Adventure.PlayerBuilder.PlayerWithName
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils.Error

class GameStatus(
  universe: Universe,
  currentStep: Step,
  currentAction: Maybe[Action],
  player: Maybe[PlayerTrait],
  renderer: Renderer
) {
  def consume(input: Input): IO[Error, GameStatus] = {
    updatePlayer(input)
      .flatMap { case (i, g) => g updateWith i }
  }

  private def updateWith(input: Maybe[Input]): IO[Error, GameStatus] = {
    input match {
      case Maybe.Just(i) => this updateWith i
      case Maybe.Empty() => IO.sync(this)
    }
  }

  private def updateWith(input: Input): IO[Error, GameStatus] = {
    this match {
      case GameStatus(_, _, Maybe.Just(a), Maybe.Just(p: Player), _) =>
        this.applyAction(a, p)
      case GameStatus(_, s, Maybe.Empty(), Maybe.Just(p: Player), _) =>
        this.selectAction(input, s, p)
    }
  }

  private def applyAction(a: Action, p: Player): IO[Error, GameStatus] = {
    for {
      newStep <- a.getStep
      player <- p.addHistory(newStep)
      game <- this.withStep(newStep)
      game <- game.withPlayer(player)
      game <- game.removeAction()
    } yield {
      game
    }
  }

  private def selectAction(input: Input, s: Step, p: Player): IO[Error, GameStatus] = {
    for {
      actions <- s.getActions(p)
      inputData <- Input.getContent(input)
      action <- actions.getAction(inputData.input)
      game <- this.withAction(action)
    } yield {
      game
    }
  }

  private def updatePlayer(input: Input): IO[Error, (Maybe[Input], GameStatus)] = {
    this.player match {
      case Maybe.Empty() => for {
        inputData <- Input.getContent(input)
        player <- PlayerBuilder.create(inputData.input)
        game <- this.withPlayer(player)
      } yield {
        (Maybe.empty[Input], game)
      }
      case Maybe.Just(p: PlayerWithName) =>
        for {
          inputData <- Input.getContent(input)
          player <- p.selectRace(this.universe.getAvailableRaces)(inputData.input)
          game <- this.withPlayer(player)
        } yield {
          (Maybe.empty[Input], game)
        }
      case _ => IO.sync((Maybe.just(input), this))
    }
  }

  def getNextMessage: IO[Error, TerminalMessage] = {
    this match {
      case GameStatus(_, s, Maybe.Empty(), Maybe.Just(p: Player), r) => r.display(s, Maybe.Just(p))
      case GameStatus(_, _, _, p, r) => IO.sync(r.display(p))
      case _ => IO.fail(Error("End State", "No more state"))
    }
  }

  private def copy(
    universe: Maybe[Universe] = Maybe.empty,
    currentStep: Maybe[Step] = Maybe.empty,
    currentAction: Maybe[Action] = Maybe.empty,
    player: Maybe[PlayerTrait] = Maybe.empty,
    renderer: Maybe[Renderer] = Maybe.empty
  ): GameStatus = {
    new GameStatus(
      universe = universe.getOrElse(this.universe),
      currentStep = currentStep.getOrElse(this.currentStep),
      currentAction = currentAction.orElse(this.currentAction),
      player = player.orElse(this.player),
      renderer = renderer.getOrElse(this.renderer)
    )
  }

  def withPlayer(p: PlayerTrait): IO[Error, GameStatus] = {
    IO.sync(this.copy(player = Maybe.Just(p)))
  }

  def withStep(step: Step): IO[Error, GameStatus] = {
    IO.sync(this.copy(currentStep = Maybe.Just(step)))
  }

  def withAction(a: Action): IO[Error, GameStatus] = {
    IO.sync(this.copy(currentAction = Maybe.Just(a)))
  }

  def removeAction(): IO[Error, GameStatus] = {
    IO.sync(this.copy(currentAction = Maybe.empty))
  }

  def getPlayer: Maybe[PlayerTrait] = this.player

  protected def getUniverse: Universe = this.universe

  protected def getCurrentAction: Maybe[Action] = this.currentAction

  protected def getCurrentStep: Step = this.currentStep

  protected def getRenderer: Renderer = this.renderer
}

object GameStatus {

  def unapply(
    arg: GameStatus
  ): Option[(Universe, Step, Maybe[Action], Maybe[PlayerTrait], Renderer)] = {
    Some(arg.getUniverse, arg.getCurrentStep, arg.getCurrentAction, arg.getPlayer, arg.getRenderer)
  }

  def apply(universe: Universe): GameStatus = {
    new GameStatus(universe, universe.getStartingStep, Maybe.empty, Maybe.empty, Renderer())
  }
}
