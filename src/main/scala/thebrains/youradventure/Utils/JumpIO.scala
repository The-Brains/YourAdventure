package thebrains.youradventure.Utils

import scalaz.zio.IO
import thebrains.youradventure.FPTerminalIO.Input
import thebrains.youradventure.Game.GameStatus

object JumpIO {

  case class CMsg(
    input:    Input,
    game:     GameStatus,
    passNext: Boolean
  ) {
    def toStable: CMsgS = CMsgS(input, game)
  }

  case class CMsgS(
    input: Input,
    game:  GameStatus
  )

  def debugPrint(txt: String): Unit = if (false) println(s"[DEBUG] $txt")

  implicit class JumpIOImpl(io: IO[Err, CMsg]) {
    def mightJumpNext(next: CMsgS => IO[Err, CMsgS]): IO[Err, CMsgS] = {
      io.flatMap {
        case msg @ CMsg(_, _, false) =>
          debugPrint("mightJumpNext: test: False")
          next(msg.toStable)
        case msg @ CMsg(_, _, true) =>
          debugPrint("mightJumpNext: test: True")
          IO.sync(msg.toStable)
      }
    }

    def mightJumpNextChain(next: CMsgS => IO[Err, CMsg]): IO[Err, CMsg] = {
      io.flatMap {
        case msg @ CMsg(_, _, false) =>
          debugPrint("mightJumpNextChain: test: False")
          next(msg.toStable)
        case msg @ CMsg(_, _, true) =>
          debugPrint("mightJumpNextChain: test: True")
          IO.sync(msg)

      }
    }

    def tailChain(next: CMsgS => IO[Err, GameStatus]): IO[Err, GameStatus] = {
      io.flatMap {
        case msg @ CMsg(_, _, false) =>
          debugPrint("tailChain: test: False")
          next(msg.toStable)
        case msg @ CMsg(_, _, true) =>
          debugPrint("tailChain: test: True")
          IO.sync(msg.game)
      }
    }
  }

}
