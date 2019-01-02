package thebrains.youradventure.FPTerminalIO

import scalaz.zio.IO
import thebrains.youradventure.Utils.Error

sealed class Input

final case class InputFilled(input: String) extends Input

final case object InputEmpty extends Input

object Input {
  def getContent(input: Input): IO[Error, InputFilled] = {
    input match {
      case i: InputFilled => IO.sync(i)
      case InputEmpty => IO.fail(Error("Empty Input", "Was expecting filled input but got nothing"))
    }
  }
}
