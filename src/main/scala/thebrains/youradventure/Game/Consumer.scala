package thebrains.youradventure.Game

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.Action
import thebrains.youradventure.Adventure.PlayerBuilder.PlayerWithName
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FPTerminalIO._
import thebrains.youradventure.Utils.Error

private[Game] class Consumer(
  game:    GameStatus,
  updater: Updater
) {

  def consume(input: Input): IO[Error, GameStatus] = {
    this
      .consumeError(input)
      .flatMap {
        case (i: InputFilled, g) =>
          g.consumer
            .updatePlayer(i)
            .flatMap {
              case (i: InputFilled, gg) => gg.consumer updateWith i
              case (InputEmpty, gg) => IO.sync(gg)
            }
        case (InputEmpty, g) => IO.sync(g)
      }
  }

  private def consumeError(input: Input): IO[Error, (Input, GameStatus)] = {
    game.getCurrentError match {
      case Maybe.Just(_) => IO.sync((InputEmpty, updater.removeError()))
      case Maybe.Empty() => IO.sync((input, game))
    }
  }

  private def updateWith(input: Input): IO[Error, GameStatus] = {
    game match {
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
      game    <- updater.withStep(newStep)
      game    <- game.updater.withPlayer(player)
      game    <- game.updater.removeAction()
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
      game      <- updater.withAction(action)
    } yield {
      game
    }
  }

  private def updatePlayer(input: Input): IO[Error, (Input, GameStatus)] = {
    game.getPlayer match {
      case Maybe.Empty() =>
        for {
          inputData <- Input.getContent(input)
          player    <- PlayerBuilder.create(inputData.input)
          game      <- updater.withPlayer(player)
        } yield {
          (InputEmpty, game)
        }
      case Maybe.Just(p: PlayerWithName) =>
        (for {
          inputData <- Input.getContent(input)
          player    <- p.selectRace(game.getUniverse.getAvailableRaces)(inputData.input)
          game      <- updater.withPlayer(player)
        } yield {
          (InputEmpty, game)
        }).attempt
          .map {
            case Left(error)   => (InputEmpty, updater.withError(error))
            case Right((i, g)) => (i, g)
          }
      case _ => IO.sync((input, game))
    }
  }
}
