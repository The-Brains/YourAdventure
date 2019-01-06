package thebrains.youradventure.FPTerminalIO

import scalaz.zio.IO
import thebrains.youradventure.Utils.{Err, ErrorIO}

sealed class Input

final case class InputFilled(input: String) extends Input

case object InputEmpty extends Input

object Input {
  def getContent(input: Input): IO[Err, InputFilled] = {
    input match {
      case i: InputFilled => IO.sync(i)
      case InputEmpty => ErrorIO("Empty Input", "Was expecting filled input but got nothing")
    }
  }
}
