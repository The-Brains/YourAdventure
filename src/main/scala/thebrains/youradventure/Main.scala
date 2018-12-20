package thebrains.youradventure

import scalaz.zio.{App, IO}
import thebrains.youradventure.FPTerminalIO.TerminalPrint
import thebrains.youradventure.Utils.Error

object Main extends App {
  override def run(args: List[String]): IO[Nothing, ExitStatus] = {
    myAppLogic(TerminalPrint()).attempt.map(_.fold(_ => 1, _ => 0)).map(ExitStatus.ExitNow(_))
  }

  def myAppLogic(tp: TerminalPrint): IO[Error, Unit] = {
    for {
      n <- tp.askText("Hello! What is your name?")
      _ <- tp.printToConsole("Hello, " + n + ", good to meet you!")
    } yield {
      ()
    }
  }
}
