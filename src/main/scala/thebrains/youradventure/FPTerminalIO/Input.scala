package thebrains.youradventure.FPTerminalIO

import scalaz.zio.IO
import thebrains.youradventure.Utils.{Err, ErrorIO}

sealed trait Input {
  def getContent: IO[Err, InputFilled]
}

final case class InputFilled(input: String) extends Input {
  def getContent: IO[Err, InputFilled] = {
    IO.sync(this)
  }
}

case object InputEmpty extends Input {
  def getContent: IO[Err, InputFilled] = {
    ErrorIO("Empty Input", "Was expecting filled input but got nothing")
  }
}

object Input {
  def apply(): Input = InputEmpty
  def apply(message: String): Input = InputFilled(message)
}
