package thebrains.youradventure.Game

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.PlayerBuilder.PlayerWithName
import thebrains.youradventure.Adventure.StepPack.Step
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FPTerminalIO._
import thebrains.youradventure.Utils.Error

private[Game] class Consumer(
  game:    GameStatus,
  updater: Updater
) {

  import Consumer._

  def consume(input: Input): IO[Error, GameStatus] = {
    this
      .consumeError(input)
      .mightJumpNextChain { case CMsgS(i, g) => g.consumer.endGame(i) }
      .mightJumpNextChain { case CMsgS(i, g) => g.consumer.updatePlayer(i) }
      .tailChain { case CMsgS(i, g) => g.consumer.updateWith(i) }
  }

  private def endGame(i: Input): IO[Error, CMsg] = {
    game.getCurrentStep match {
      case Maybe.Just(_) => IO.sync(CMsg(i, game, passNext = false))
      case Maybe.Empty() => IO.sync(CMsg(InputEmpty, game, passNext = true))
    }
  }

  private def consumeError(input: Input): IO[Error, CMsg] = {
    game.getCurrentError match {
      case Maybe.Just(_) =>
        IO.sync(CMsg(InputEmpty, updater.removeError(), passNext = true))
      case Maybe.Empty() => IO.sync(CMsg(input, game, passNext = false))
    }
  }

  private def updateWith(input: Input): IO[Error, GameStatus] = {
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
  ): IO[Error, GameStatus] = {
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
  ): IO[Error, GameStatus] = {
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
  ): IO[Error, GameStatus] = {
    for {
      inputData <- Input.getContent(input)
      action    <- actions.getAction(inputData.input)
      game      <- updater.withAction(action)
    } yield {
      game
    }
  }

  private def updatePlayer(input: Input): IO[Error, CMsg] = {
    game.getPlayer match {
      case Maybe.Empty() =>
        for {
          inputData <- Input.getContent(input)
          player    <- PlayerBuilder.create(inputData.input)
          game      <- updater.withPlayer(player)
        } yield {
          CMsg(InputEmpty, game, passNext = true)
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
            case Left(error)   => CMsg(InputEmpty, updater.withError(error), passNext = true)
            case Right((i, g)) => CMsg(i, g, passNext = true)
          }
      case _ => IO.sync(CMsg(input, game, passNext = false))
    }
  }
}

object Consumer {

  private case class CMsg(
    input:    Input,
    game:     GameStatus,
    passNext: Boolean
  ) {
    def toStable: CMsgS = CMsgS(input, game)
  }

  private case class CMsgS(
    input: Input,
    game:  GameStatus
  )

  implicit private class JumpIO(io: IO[Error, CMsg]) {
    def mightJumpNext(next: CMsgS => IO[Error, CMsgS]): IO[Error, CMsgS] = {
      io.flatMap {
        case CMsg(_, _, test) =>
          if (test) {
            io.map(_.toStable)
          } else {
            io.map(_.toStable).flatMap(next)
          }
      }
    }

    def mightJumpNextChain(next: CMsgS => IO[Error, CMsg]): IO[Error, CMsg] = {
      io.flatMap {
        case CMsg(_, _, test) =>
          if (test) {
            io
          } else {
            io.map(_.toStable).flatMap(next)
          }
      }
    }

    def tailChain(next: CMsgS => IO[Error, GameStatus]): IO[Error, GameStatus] = {
      io.flatMap {
        case CMsg(_, _, test) =>
          if (test) {
            io.map { case CMsg(_, g, _) => g }
          } else {
            io.map(_.toStable).flatMap(next)
          }
      }
    }
  }

}
