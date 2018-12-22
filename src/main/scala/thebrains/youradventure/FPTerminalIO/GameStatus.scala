package thebrains.youradventure.FPTerminalIO

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.Action
import thebrains.youradventure.Adventure.PlayerBuilder.PlayerWithName
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils.Error

//scalastyle:off
class GameStatus(
  universe:      Universe,
  currentStep:   Step,
  currentAction: Maybe[Action],
  player:        Maybe[PlayerTrait],
  renderer:      Renderer,
  currentError:  Maybe[Error]
) {
  override def toString: String = {
    s"State(" +
      s"step:${currentStep.toString}, " +
      s"action:${currentAction.toString}, " +
      s"player:${player.toString}, " +
      s"error:${currentError.toString}" +
      s")"
  }

  def consume(input: Input): IO[Error, GameStatus] = {
    this
      .consumeError(input)
      .flatMap {
        case (i: InputFilled, game) =>
          game
            .updatePlayer(i)
            .flatMap {
              case (i: InputFilled, g) => g updateWith i
              case (InputEmpty, g) => IO.sync(g)
            }
        case (InputEmpty, game) => IO.sync(game)
      }
  }

  private def consumeError(input: Input): IO[Error, (Input, GameStatus)] = {
    this.currentError match {
      case Maybe.Just(_) => IO.sync((InputEmpty, this.removeError()))
      case Maybe.Empty() => IO.sync((input, this))
    }
  }

  private def updateWith(input: Input): IO[Error, GameStatus] = {
    this match {
      case GameStatus(_, _, _, Maybe.Just(a), Maybe.Just(p: Player), _) =>
        this.applyAction(a, p)
      case GameStatus(_, _, s, Maybe.Empty(), Maybe.Just(p: Player), _) =>
        this.selectAction(input, s, p)
    }
  }

  private def applyAction(
    a: Action,
    p: Player
  ): IO[Error, GameStatus] = {
    for {
      newStep <- a.getStep
      player  <- p.addHistory(newStep)
      game    <- this.withStep(newStep)
      game    <- game.withPlayer(player)
      game    <- game.removeAction()
    } yield {
      game
    }
  }

  private def selectAction(
    input: Input,
    s:     Step,
    p:     Player
  ): IO[Error, GameStatus] = {
    for {
      actions   <- s.getActions(p)
      inputData <- Input.getContent(input)
      action    <- actions.getAction(inputData.input)
      game      <- this.withAction(action)
    } yield {
      game
    }
  }

  private def updatePlayer(input: Input): IO[Error, (Input, GameStatus)] = {
    this.player match {
      case Maybe.Empty() =>
        for {
          inputData <- Input.getContent(input)
          player    <- PlayerBuilder.create(inputData.input)
          game      <- this.withPlayer(player)
        } yield {
          (InputEmpty, game)
        }
      case Maybe.Just(p: PlayerWithName) =>
        (for {
          inputData <- Input.getContent(input)
          player    <- p.selectRace(this.universe.getAvailableRaces)(inputData.input)
          game      <- this.withPlayer(player)
        } yield {
          (InputEmpty, game)
        }).attempt
          .map {
            case Left(error)   => (InputEmpty, this.withError(error))
            case Right((i, g)) => (i, g)
          }
      case _ => IO.sync((input, this))
    }
  }

  def getNextMessage: IO[Error, TerminalMessage] = {
    this match {
      case GameStatus(_, Maybe.Just(error), _, _, _, r) => r.display(error)
      case GameStatus(_, _, s, Maybe.Empty(), Maybe.Just(p: Player), r) =>
        r.display(s, Maybe.Just(p))
      case GameStatus(_, _, _, _, p, r) => IO.sync(r.display(p))
      case _                            => IO.fail(Error("End State", "No more state"))
    }
  }

  private def copy(
    universe:      Maybe[Universe] = Maybe.empty,
    currentStep:   Maybe[Step] = Maybe.empty,
    currentAction: Maybe[Action] = Maybe.empty,
    player:        Maybe[PlayerTrait] = Maybe.empty,
    renderer:      Maybe[Renderer] = Maybe.empty,
    currentError:  Maybe[Error] = Maybe.empty
  ): GameStatus = {
    new GameStatus(
      universe = universe.getOrElse(this.universe),
      currentStep = currentStep.getOrElse(this.currentStep),
      currentAction = currentAction.orElse(this.currentAction),
      player = player.orElse(this.player),
      renderer = renderer.getOrElse(this.renderer),
      currentError = currentError.orElse(this.currentError)
    )
  }

  def withError(error: Error): GameStatus = {
    this.copy(currentError = Maybe.Just(error))
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
    IO.sync(
      new GameStatus(
        universe = this.universe,
        currentStep = this.currentStep,
        currentAction = Maybe.empty,
        player = this.player,
        renderer = this.renderer,
        currentError = this.currentError
      )
    )
  }

  def removeError(): GameStatus = {
    new GameStatus(
      universe = this.universe,
      currentStep = this.currentStep,
      currentAction = this.currentAction,
      player = this.player,
      renderer = this.renderer,
      currentError = Maybe.empty
    )
  }

  def getPlayer: Maybe[PlayerTrait] = this.player

  protected def getUniverse: Universe = this.universe

  protected def getCurrentAction: Maybe[Action] = this.currentAction

  protected def getCurrentStep: Step = this.currentStep

  protected def getRenderer: Renderer = this.renderer

  protected def getError: Maybe[Error] = this.currentError
}

object GameStatus {

  def unapply(
    arg: GameStatus
  ): Option[(Universe, Maybe[Error], Step, Maybe[Action], Maybe[PlayerTrait], Renderer)] = {
    Some(
      arg.getUniverse,
      arg.getError,
      arg.getCurrentStep,
      arg.getCurrentAction,
      arg.getPlayer,
      arg.getRenderer
    )
  }

  def apply(universe: Universe): GameStatus = {
    new GameStatus(
      universe,
      universe.getStartingStep,
      Maybe.empty,
      Maybe.empty,
      Renderer(),
      Maybe.empty
    )
  }
}
