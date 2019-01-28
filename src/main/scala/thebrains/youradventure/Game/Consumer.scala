package thebrains.youradventure.Game

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.PlayerBuilder.PlayerWithName
import thebrains.youradventure.Adventure.StepPack.Step
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FPTerminalIO._
import thebrains.youradventure.Utils.Err

private[Game] class Consumer(
  game:    GameStatus,
  updater: Updater
) {

  import thebrains.youradventure.Utils.JumpIO._

  def consume(input: Input): IO[Err, GameStatus] = {
    debugPrint(s"Consume with input: '$input' and with game: '$game'.")
    this
      .consumeError(input)
      .mightJumpNextChain { case CMsgS(i, g) => g.consumer.endGame(i) }
      .mightJumpNextChain { case CMsgS(i, g) => g.consumer.updatePlayer(i) }
      .tailChain { case CMsgS(i, g) => g.consumer.updateWith(i) }
      .attempt
      .flatMap {
        case Left(error) =>
          if (error.isFatal) {
            error.toIO
          } else {
            IO.sync(game.updater.withError(error))
          }
        case Right(g) => IO.sync(g)
      }
  }

  private def consumeError(input: Input): IO[Err, CMsg] = {
    debugPrint(s"Consume error")
    game.getCurrentError match {
      case Maybe.Just(_) =>
        IO.sync(CMsg(InputEmpty, updater.removeError(), passNext = true))
      case Maybe.Empty() => IO.sync(CMsg(input, game, passNext = false))
    }
  }

  private def endGame(input: Input): IO[Err, CMsg] = {
    debugPrint(s"Process endgame")
    game.getCurrentStep match {
      case Maybe.Just(_) => IO.sync(CMsg(input, game, passNext = false))
      case Maybe.Empty() => IO.sync(CMsg(InputEmpty, game, passNext = true))
    }
  }

  private def updateWith(input: Input): IO[Err, GameStatus] = {
    debugPrint(s"Process general update")
    game match {
      case GameStatus(_, _, _, Maybe.Just(a), Maybe.Just(p: Player), _) =>
        this.applyAction(a, p)
      case GameStatus(_, _, Maybe.Just(s), Maybe.Empty(), Maybe.Just(p: Player), _) =>
        pickAction(input, s, p)
    }
  }

  private def pickAction(
    input: Input,
    s:     Step,
    p:     Player
  ): IO[Err, GameStatus] = {
    debugPrint(s"pick action")
    for {
      actions <- s.getActions(p)
      g       <- if (actions.nonEmpty) selectAction(input, actions) else game.updater.removeStep()
    } yield {
      g
    }
  }

  private def applyAction(
    a: Action,
    p: Player
  ): IO[Err, GameStatus] = {
    for {
      newStep <- a.getStep(game.getUniverse.getAvailableSteps)
      player  <- p.addHistory(newStep)
      game    <- updater.withStep(newStep)
      game    <- game.updater.withPlayer(player)
      game    <- game.updater.removeAction()
    } yield {
      game
    }
  }

  private def selectAction(
    input:   Input,
    actions: ActionCollection
  ): IO[Err, GameStatus] = {
    for {
      inputData <- input.getContent
      action    <- actions.getAction(inputData.input)
      game      <- updater.withAction(action)
    } yield {
      game
    }
  }

  private def updatePlayer(input: Input): IO[Err, CMsg] = {
    debugPrint(s"update player")

    game.getPlayer match {
      case Maybe.Empty() =>
        for {
          inputData <- input.getContent
          player    <- PlayerBuilder.create(inputData.input)
          game      <- updater.withPlayer(player)
        } yield {
          CMsg(InputEmpty, game, passNext = true)
        }
      case Maybe.Just(p: PlayerWithName) =>
        for {
          inputData <- input.getContent
          player    <- p.selectRace(game.getUniverse.getAvailableRaces)(inputData.input)
          game      <- updater.withPlayer(player)
        } yield {
          CMsg(InputEmpty, game, passNext = true)
        }
      case _ => IO.sync(CMsg(input, game, passNext = false))
    }
  }

  def consumeAction(
    universe: Universe,
    a:        Action
  ): IO[Err, GameStatus] = {
    for {
      nextStep <- a.getStep(universe.getAvailableSteps)
      game     <- game.updater.withStep(nextStep)
    } yield {
      game
    }
  }
}
